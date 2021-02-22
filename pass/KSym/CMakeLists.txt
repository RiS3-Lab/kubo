add_llvm_library(KSym MODULE
		Lower.cpp
		#Alias.cpp
		IntrinsicCleaner.cpp

		Util.cpp
		Logger.cpp

		Slice.cpp
		DAG.cpp
		Unroll.cpp
		Oracle.cpp

		Symbolic.cpp
		Trace.cpp
		Libcall.cpp
		Asmcall.cpp
		SEG.cpp

		Fetch.cpp
		# Func.cpp
		AnalysisDriver.cpp
		Tool.cpp
		Propagation.cpp
		TraceStatus.cpp
		Record.cpp

		Node.cpp
		#Global.cpp
		#PA.cpp
		#CallGraph
)

set_target_properties(KSym PROPERTIES COMPILE_FLAGS "-fno-rtti -fPIC")
