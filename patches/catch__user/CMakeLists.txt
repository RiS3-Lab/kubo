#set(LLVM_LINK_COMPONENTS
#  Support
#  )

add_clang_executable(catch__user traverseAST.cpp)

clang_target_link_libraries(catch__user
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  clangToolingASTDiff
  clangFrontendTool
  )

#target_link_libraries(kubo clangAST clangBasic clangFrontend   clangTooling)
