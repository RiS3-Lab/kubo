include_directories(include/ include/SCC/ ../Utils/include ../Customizations/include ../LinuxKernelCustomizations/include
        ../RangeAnalysis/include)

file(GLOB SAA_SRCS
        "src/*.cpp"
        "src/SCC/*.c"
        "src/bug_detectors/*.cpp"
        "src/bug_detectors/warnings/*.cpp"
        "include/*.h"
        "include/SCC/*.h"
        "include/bug_detectors/*.h"
        "include/bug_detectors/warnings/*.h"
        )

add_library(SoundyAliasAnalysis MODULE
        # List your source files here.
        ${SAA_SRCS})


target_link_libraries(SoundyAliasAnalysis Utils Customizations LinuxKernelCustomizations RangeAnalysis)


# Use C++11 to compile our pass (i.e., supply -std=c++11).
target_compile_features(SoundyAliasAnalysis PRIVATE cxx_range_for cxx_auto_type)

# LLVM is (typically) built with no C++ RTTI. We need to match that;
# otherwise, we'll get linker errors about missing RTTI data.
set_target_properties(SoundyAliasAnalysis PROPERTIES
    COMPILE_FLAGS "-g -fno-rtti"
)


# Get proper shared-library behavior (where symbols are not necessarily
# resolved when the shared library is linked) on OS X.
if(APPLE)
    set_target_properties(SoundyAliasAnalysis PROPERTIES
        LINK_FLAGS "-undefined dynamic_lookup"
    )
endif(APPLE)


