# Tau-Argus GHM: hypercube (GHMITER) secondary suppression.
#
# In-process C port of the GHMITER 4 Fortran program (Gießing/Repsilber;
# original source kept under reference/fortran, EUPL 1.1,
# sdcTools/hypercube). It replaces the legacy external GHMITER
# executable: the tauargus core writes, per table, a steering file
# and a cells file, this library runs the algorithm and writes the
# result file (leading token 1129 per cell = secondarily unsafe),
# which the core reads back.
#
# The algorithm is self-contained: no solver dependency (the legacy
# program called no external LP solver either).

cmake_minimum_required(VERSION 3.16)

project(tauargus_ghm C)

set(GHM_SRC
    src/ghm.c
    src/ghm_cells.c
    src/ghm_steering.c
    src/ghm_iter.c
)

option(BUILD_SHARED_LIBS "Build shared native libraries" ON)
add_library(tauargus_ghm ${GHM_SRC})
add_library(tauargus::ghm ALIAS tauargus_ghm)
if(NOT BUILD_SHARED_LIBS)
    target_compile_definitions(tauargus_ghm PUBLIC TAUARGUS_STATIC)
endif()

target_include_directories(tauargus_ghm
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
)

target_compile_options(tauargus_ghm PRIVATE
    $<$<C_COMPILER_ID:GNU,Clang,AppleClang>:-Wall -Wextra>
)

target_compile_features(tauargus_ghm PUBLIC c_std_99)

# The algorithm uses log() for its information-loss cost.
find_library(GHM_MATH_LIB m)
if(GHM_MATH_LIB)
    target_link_libraries(tauargus_ghm PUBLIC ${GHM_MATH_LIB})
endif()

# Export all symbols from the Windows DLL (the module exposes a small
# plain C API).
if(WIN32)
    set_target_properties(tauargus_ghm PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
endif()

install(TARGETS tauargus_ghm
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Smoke tests (run by the parent repo's dev loop,
# scripts/dev-test.sh).
option(TAUARGUS_GHM_BUILD_TESTS "Build ghm smoke tests" OFF)
if(TAUARGUS_GHM_BUILD_TESTS)
    enable_testing()
    add_executable(ghm_smoke_test tests/ghm_smoke_test.c)
    target_link_libraries(ghm_smoke_test PRIVATE tauargus_ghm)
    add_test(NAME ghm_smoke COMMAND ghm_smoke_test)

    add_executable(ghm_cells_test tests/ghm_cells_test.c)
    target_link_libraries(ghm_cells_test PRIVATE tauargus_ghm)
    add_test(NAME ghm_cells COMMAND ghm_cells_test)

    add_executable(ghm_steering_test tests/ghm_steering_test.c)
    target_link_libraries(ghm_steering_test PRIVATE tauargus_ghm)
    add_test(NAME ghm_steering COMMAND ghm_steering_test)

    add_executable(ghm_iter_test tests/ghm_iter_test.c)
    target_link_libraries(ghm_iter_test PRIVATE tauargus_ghm)
    add_test(NAME ghm_iter COMMAND ghm_iter_test)

    add_executable(ghm_validation_test tests/ghm_validation_test.c)
    target_link_libraries(ghm_validation_test PRIVATE tauargus_ghm)
    add_test(NAME ghm_validation COMMAND ghm_validation_test)
endif()
