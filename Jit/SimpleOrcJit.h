#pragma once

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/OrcError.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/IR/Mangler.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Scalar.h>

#include <functional>
#include <memory>
#include <vector>

#define DEBUG_TYPE "jitfromscratch"

extern int Py_JITDebugFlag;

using llvm::Mangler;
using llvm::raw_string_ostream;
using llvm::RTDyldMemoryManager;
using llvm::orc::RTDyldObjectLinkingLayer;
using llvm::Error;
using llvm::orc::createLegacyLookupResolver;

static inline
void llvm_module_to_file(const llvm::Module& module, const char* filename) {
  std::string str;
  llvm::raw_string_ostream os(str);
  module.print(os, nullptr);

  std::ofstream of(filename);
  of << os.str();
}

typedef std::function<void(void*)> SetStackmapSectionAddressFunction;

class CustomMemoryManager : public llvm::SectionMemoryManager {
public:
    SetStackmapSectionAddressFunction set_stackmap_section_address_;
    CustomMemoryManager(SetStackmapSectionAddressFunction set_stackmaps_address) :
        llvm::SectionMemoryManager(),
        set_stackmap_section_address_(set_stackmaps_address) {
    }
    CustomMemoryManager(const CustomMemoryManager &) = delete;
    void operator=(const CustomMemoryManager &) = delete;
    ~CustomMemoryManager() override { }

    uint8_t *allocateDataSection(uintptr_t Size,
                                 unsigned Alignment,
                                 unsigned SectionID,
                                 llvm::StringRef SectionName,
                                 bool IsReadOnly) {
        uint8_t *ret = llvm::SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID, SectionName, IsReadOnly);
        if (SectionName == ".llvm_stackmaps") {
            set_stackmap_section_address_(ret);
        }
        return ret;
    }
};

class SimpleOrcJit {
  using ModulePtr_t = std::unique_ptr<llvm::Module>;
  using IRCompiler_t = llvm::orc::SimpleCompiler;

  using Optimize_f = std::function<ModulePtr_t(ModulePtr_t)>;

  using ObjectLayer_t = llvm::orc::RTDyldObjectLinkingLayer;
  using CompileLayer_t = llvm::orc::IRCompileLayer<ObjectLayer_t, IRCompiler_t>;
  using OptimizeLayer_t =
      llvm::orc::IRTransformLayer<CompileLayer_t, Optimize_f>;

  void *stackmap_section_address_;
  llvm::orc::ExecutionSession ES;
  std::shared_ptr<llvm::orc::SymbolResolver> Resolver;
  llvm::DataLayout DL;
  ObjectLayer_t ObjectLayer;
  CompileLayer_t CompileLayer;
  OptimizeLayer_t OptimizeLayer;

public:
  SimpleOrcJit(llvm::TargetMachine &targetMachine)
      : stackmap_section_address_(nullptr),
        DL(targetMachine.createDataLayout()),
        Resolver(createLegacyLookupResolver(
            [this](const std::string &Name) -> llvm::JITSymbol {
              if (auto Sym = CompileLayer.findSymbol(Name, false))
                return Sym;
              else if (auto Err = Sym.takeError())
                return std::move(Err);
              if (auto SymAddr =
                      RTDyldMemoryManager::getSymbolAddressInProcess(Name))
                return llvm::JITSymbol(SymAddr, llvm::JITSymbolFlags::Exported);
              return nullptr;
            },
            [](Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
        ObjectLayer(ES,
                    [this](llvm::orc::VModuleKey) {
                      return RTDyldObjectLinkingLayer::Resources{
                          std::make_shared<CustomMemoryManager>([&](void *addr) { stackmap_section_address_ = addr; }),
                          Resolver};
                    }),
        CompileLayer(ObjectLayer, IRCompiler_t(targetMachine)),
        OptimizeLayer(CompileLayer, [this](ModulePtr_t module) {
          return optimizeModule(std::move(module));
        }) {
    // Load own executable as dynamic library.
    // Required for RTDyldMemoryManager::getSymbolAddressInProcess().
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  llvm::orc::VModuleKey addModule(ModulePtr_t module) {
    DEBUG({
      llvm::dbgs() << "Submit LLVM module:\n\n";
      llvm::dbgs() << *module.get() << "\n\n";
    });

    auto K = ES.allocateVModule();
    cantFail(OptimizeLayer.addModule(K, std::move(module)));
    return K;
  }

  void *getStackmapSectionAddress() {
      void *ret = stackmap_section_address_;
      stackmap_section_address_ = nullptr;
      return ret;
  }

private:
  ModulePtr_t optimizeModule(ModulePtr_t module) {
    using namespace llvm;

    if (Py_JITDebugFlag > 2) {
        llvm_module_to_file(*module, "/tmp/before.ll");
        std::cout << "Module dumped to /tmp/before.ll" << std::endl;
    }

    PassManagerBuilder PMBuilder;
    PMBuilder.LoopVectorize = true;
    PMBuilder.SLPVectorize = true;
    PMBuilder.VerifyInput = true;
    PMBuilder.VerifyOutput = true;

    legacy::FunctionPassManager perFunctionPasses(module.get());
    PMBuilder.populateFunctionPassManager(perFunctionPasses);

    perFunctionPasses.doInitialization();

    for (Function &function : *module)
      perFunctionPasses.run(function);

    perFunctionPasses.doFinalization();

    legacy::PassManager perModulePasses;
    PMBuilder.populateModulePassManager(perModulePasses);
    perModulePasses.run(*module);

    DEBUG({
      outs() << "Optimized module:\n\n";
      outs() << *module.get() << "\n\n";
    });

    if (Py_JITDebugFlag > 2) {
        llvm_module_to_file(*module, "/tmp/after.ll");
        std::cout << "Module dumped to /tmp/after.ll" << std::endl;
    }

    return module;
  }

public:

  llvm::JITSymbol findSymbolInJITedCode(const std::string Name) {
    std::string MangledName;
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    return CompileLayer.findSymbol(MangledNameStream.str(), /*exported only*/ false);
  }

  llvm::JITSymbol findSymbol(const std::string Name) {
    std::string MangledName;
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    return CompileLayer.findSymbol(MangledNameStream.str(), true);
  }

  llvm::JITTargetAddress getSymbolAddress(const std::string Name) {
    return cantFail(findSymbol(Name).getAddress());
  }

  void removeModule(llvm::orc::VModuleKey K) {
    cantFail(CompileLayer.removeModule(K));
  }
};
