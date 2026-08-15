#!/usr/bin/env bash
#
# Build pasls using FPC directly (without lazbuild/lpk dependencies)
#
# Usage: build_fpc.sh [OPTIONS] [TARGET...]
#
# Options:
#   --sqlite    Enable SQLite support (adds -dUSE_SQLITE)
#   --tests     Also build testlsp (run separately with build/TARGET/testlsp)
#   --help      Show this help message
#
# Target groups:
#   (default)   Build for current platform only (native)
#   all         Build ALL targets (requires cross-compilers)
#   linux       Build Linux targets (x86_64, aarch64)
#   windows     Build Windows targets (i386, x86_64)
#   darwin      Build macOS targets
#
# Environment variables:
#   LAZARUSDIR   Path to Lazarus source directory (for CodeTools)
#   FPC          Path to FPC compiler
#   FPC_CFG      Path to FPC configuration file (should be auto-detected)
#   FPCUPDELUXE  Path to fpcupdeluxe installation (auto-detects FPC and Lazarus)
#   FPC_CROSS_UNITS  Additional unit search path for cross-compilation RTL
#
# Note: Cross-compilation requires FPC cross-compilers installed.
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUTDIR="$SCRIPT_DIR/../dist"
BUILDDIR="$SCRIPT_DIR/../build"
PROGRAM="$SCRIPT_DIR/standard/pasls.lpr"

# Colors for output (disabled on non-tty)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    NC='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    NC=''
fi

# Group definitions
LINUX_TARGETS="x86_64-linux aarch64-linux"
WINDOWS_TARGETS="i386-win32 x86_64-win64"
DARWIN_TARGETS="x86_64-darwin aarch64-darwin"

#=============================================================================
# Help must be available before any detection (which might fail)
#=============================================================================
show_help() {
    echo "Usage: $0 [OPTIONS] [TARGET...]"
    echo ""
    echo "Build pasls using FPC directly (without lazbuild/lpk dependencies)"
    echo ""
    echo "Options:"
    echo "  --sqlite    Enable SQLite support (adds -dUSE_SQLITE)"
    echo "  --tests     Also build testlsp (run separately with build/TARGET/testlsp)"
    echo "  --help      Show this help message"
    echo ""
    echo "Target groups:"
    echo "  (default)   Build for current platform only (native)"
    echo "  all         Build ALL targets (requires cross-compilers)"
    echo "  linux       Build Linux targets (x86_64, aarch64)"
    echo "  windows     Build Windows targets (i386, x86_64)"
    echo "  darwin      Build macOS targets"
    echo ""
    echo "Individual targets:"
    echo "  x86_64-linux     Linux x86_64"
    echo "  aarch64-linux    Linux ARM64"
    echo "  i386-win32       Windows 32-bit"
    echo "  x86_64-win64     Windows 64-bit"
    echo "  x86_64-darwin    macOS Intel"
    echo "  aarch64-darwin   macOS Apple Silicon"
    echo ""
    echo "Environment variables:"
    echo "  LAZARUSDIR   Path to Lazarus source directory (for CodeTools)"
    echo "  FPC          Path to FPC compiler"
    echo "  FPC_CFG      Path to FPC configuration file (should be auto-detected)"
    echo "  FPCUPDELUXE  Path to fpcupdeluxe installation"
    echo "               (auto-detects FPC and Lazarus from this)"
    echo ""
    echo "Examples:"
    echo "  $0                              # Build for current platform"
    echo "  $0 --sqlite                     # Build with SQLite support"
    echo "  $0 all                          # Build all targets (cross-compile)"
    echo "  $0 x86_64-linux                 # Build specific target"
    echo "  LAZARUSDIR=/opt/lazarus $0"
    echo "  FPCUPDELUXE=~/fpcupdeluxe $0    # Use fpcupdeluxe installation"
    echo ""
    echo "Note: Cross-compilation requires FPC cross-compilers installed."
    echo "Native builds (matching host platform) do not require cross-compilers."
}

# Check for --help early (before any detection that might fail)
for arg in "$@"; do
    if [ "$arg" = "--help" ] || [ "$arg" = "-h" ]; then
        show_help
        exit 0
    fi
done

#=============================================================================
# Platform detection
#=============================================================================
detect_host_platform() {
    local os cpu

    case "$(uname -s)" in
        Linux*)  os="linux" ;;
        Darwin*) os="darwin" ;;
        MINGW*|MSYS*|CYGWIN*) os="win64" ;;
        *)       os="unknown" ;;
    esac

    case "$(uname -m)" in
        x86_64|amd64)  cpu="x86_64" ;;
        aarch64|arm64) cpu="aarch64" ;;
        i386|i686)     cpu="i386"; [ "$os" = "win64" ] && os="win32" ;;
        *)             cpu="unknown" ;;
    esac

    echo "$cpu-$os"
}

HOST_PLATFORM=$(detect_host_platform)
HOST_OS=$(echo "$HOST_PLATFORM" | cut -d- -f2)
HOST_CPU=$(echo "$HOST_PLATFORM" | cut -d- -f1)

# Convert Unix path to Windows path (for FPC on Windows)
# FPC is a native Windows executable and doesn't understand Unix-style paths
to_windows_path() {
    local path="$1"
    if [[ "$HOST_OS" == win* ]] && command -v cygpath &> /dev/null; then
        cygpath -w "$path"
    else
        echo "$path"
    fi
}

#=============================================================================
# FPC compiler detection
#=============================================================================
# fpcupdeluxe installs fpc with fpc.cfg in the same bin directory.
# The fpc.cfg contains RTL unit paths (-Fu) that are required for compilation.
# We detect both fpc and fpc.cfg, then pass @fpc.cfg to the compiler.
#
# On Linux/macOS, fpcupdeluxe also creates fpc.sh wrapper that includes @fpc.cfg.
# We prefer fpc.sh if available, otherwise use fpc with explicit @fpc.cfg.
#=============================================================================

detect_fpc() {
    # 1. Explicit FPC environment variable
    if [ -n "$FPC" ] && [ -x "$FPC" ]; then
        # Check for fpc.cfg in the same directory
        local fpc_dir=$(dirname "$FPC")
        if [ ! -n "$FPC_CFG" ] || [ ! -f "$FPC_CFG" ]; then
            [ -f "$fpc_dir/fpc.cfg" ] && FPC_CFG="$fpc_dir/fpc.cfg"
        fi
        echo "$FPC"
        return 0
    fi

    # Helper: find fpc in a fpcupdeluxe bin directory
    # Prefers fpc.sh (wrapper with fpc.cfg) over raw fpc binary
    # Also sets FPC_CFG if fpc.cfg exists
    find_fpc_in_dir() {
        local bin_dir="$1"
        local fpc_exe=""

        case "$HOST_OS" in
            win32|win64)
                # Windows: fpc.exe (no .bat wrapper typically)
                [ -x "$bin_dir/fpc.exe" ] && fpc_exe="$bin_dir/fpc.exe"
                ;;
            *)
                # Linux/macOS: prefer fpc.sh (wrapper with @fpc.cfg), then fpc
                if [ -x "$bin_dir/fpc.sh" ]; then
                    fpc_exe="$bin_dir/fpc.sh"
                elif [ -x "$bin_dir/fpc" ]; then
                    fpc_exe="$bin_dir/fpc"
                fi
                ;;
        esac

        if [ -n "$fpc_exe" ]; then
            # Check for fpc.cfg in the same directory
            [ -f "$bin_dir/fpc.cfg" ] && FPC_CFG="$bin_dir/fpc.cfg"
            echo "$fpc_exe"
            return 0
        fi
        return 1
    }

    # 2. fpcupdeluxe installation (explicit FPCUPDELUXE variable)
    if [ -n "$FPCUPDELUXE" ]; then
        local bin_dir="$FPCUPDELUXE/fpc/bin/${HOST_CPU}-${HOST_OS}"
        local fpc_exe
        fpc_exe=$(find_fpc_in_dir "$bin_dir")
        if [ -n "$fpc_exe" ]; then
            echo "$fpc_exe"
            return 0
        fi

        # Try other architectures
        for arch_os in "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" "i386-win32" "x86_64-win64"; do
            bin_dir="$FPCUPDELUXE/fpc/bin/$arch_os"
            fpc_exe=$(find_fpc_in_dir "$bin_dir")
            if [ -n "$fpc_exe" ]; then
                echo "$fpc_exe"
                return 0
            fi
        done
    fi

    # 3. Common fpcupdeluxe installation paths
    local fpcup_candidates=()
    case "$HOST_OS" in
        win32|win64)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "/c/fpcupdeluxe"
                "/d/fpcupdeluxe"
                "/c/dev/fpcupdeluxe"
                "/d/dev/fpcupdeluxe"
            )
            ;;
        darwin)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "$HOME/Developer/fpcupdeluxe"
            )
            ;;
        linux)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "/opt/fpcupdeluxe"
            )
            ;;
    esac

    for base in "${fpcup_candidates[@]}"; do
        local bin_dir="$base/fpc/bin/${HOST_CPU}-${HOST_OS}"
        local fpc_exe
        fpc_exe=$(find_fpc_in_dir "$bin_dir")
        if [ -n "$fpc_exe" ]; then
            echo "$fpc_exe"
            return 0
        fi

        # Search in all subdirectories
        if [ -d "$base/fpc/bin" ]; then
            for subdir in "$base/fpc/bin"/*; do
                [ -d "$subdir" ] || continue
                fpc_exe=$(find_fpc_in_dir "$subdir")
                if [ -n "$fpc_exe" ]; then
                    echo "$fpc_exe"
                    return 0
                fi
            done
        fi
    done

    # 4. System fpc in PATH
    if command -v fpc &> /dev/null; then
        local fpc_path=$(command -v fpc)
        local fpc_dir=$(dirname "$fpc_path")
        [ -f "$fpc_dir/fpc.cfg" ] && FPC_CFG="$fpc_dir/fpc.cfg"
        echo "$fpc_path"
        return 0
    fi

    echo ""
    return 1
}

#=============================================================================
# Lazarus directory detection
#=============================================================================
detect_lazarus_dir() {
    # 1. Explicit LAZARUSDIR environment variable
    if [ -n "$LAZARUSDIR" ] && [ -d "$LAZARUSDIR/components/codetools" ]; then
        echo "$LAZARUSDIR"
        return 0
    fi

    # 2. fpcupdeluxe installation
    if [ -n "$FPCUPDELUXE" ] && [ -d "$FPCUPDELUXE/lazarus/components/codetools" ]; then
        echo "$FPCUPDELUXE/lazarus"
        return 0
    fi

    # 3. Common fpcupdeluxe installation paths
    local fpcup_candidates=()
    case "$HOST_OS" in
        win32|win64)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "/c/fpcupdeluxe"
                "/d/fpcupdeluxe"
                "/c/dev/fpcupdeluxe"
                "/d/dev/fpcupdeluxe"
            )
            ;;
        darwin)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "$HOME/Developer/fpcupdeluxe"
            )
            ;;
        linux)
            fpcup_candidates=(
                "$HOME/fpcupdeluxe"
                "/opt/fpcupdeluxe"
            )
            ;;
    esac

    for base in "${fpcup_candidates[@]}"; do
        if [ -d "$base/lazarus/components/codetools" ]; then
            echo "$base/lazarus"
            return 0
        fi
    done

    # 4. Standard Lazarus installation paths
    local candidates=()
    case "$HOST_OS" in
        darwin)
            candidates=(
                "$HOME/Developer/lazarus-main"
                "$HOME/lazarus"
                "/Applications/Lazarus"
                "/usr/local/share/lazarus"
            )
            ;;
        linux)
            candidates=(
                "$HOME/lazarus"
                "/usr/share/lazarus"
                "/usr/local/share/lazarus"
                "/opt/lazarus"
            )
            ;;
        win32|win64)
            candidates=(
                "$HOME/lazarus"
                "/c/lazarus"
                "/d/lazarus"
                "$PROGRAMFILES/Lazarus"
            )
            ;;
    esac

    for dir in "${candidates[@]}"; do
        if [ -d "$dir/components/codetools" ]; then
            echo "$dir"
            return 0
        fi
    done

    echo ""
    return 1
}

#=============================================================================
# Detect FPC and Lazarus
#=============================================================================
FPC_CMD=$(detect_fpc)
if [ -z "$FPC_CMD" ]; then
    echo -e "${RED}Error: FPC compiler not found.${NC}"
    echo ""
    echo "Searched in:"
    echo "  - FPC environment variable"
    echo "  - FPCUPDELUXE directory"
    echo "  - Common fpcupdeluxe paths"
    echo "  - System PATH"
    echo ""
    echo "Solutions:"
    echo "  1. Set FPC=/path/to/fpc"
    echo "  2. Set FPCUPDELUXE=/path/to/fpcupdeluxe"
    echo "  3. Add fpc to PATH"
    exit 1
fi

LAZARUSDIR=$(detect_lazarus_dir)
if [ -z "$LAZARUSDIR" ]; then
    echo -e "${RED}Error: Lazarus directory not found.${NC}"
    echo ""
    echo "Searched in:"
    echo "  - LAZARUSDIR environment variable"
    echo "  - FPCUPDELUXE/lazarus"
    echo "  - Common fpcupdeluxe paths"
    echo "  - Standard Lazarus paths"
    echo ""
    echo "Solutions:"
    echo "  1. Set LAZARUSDIR=/path/to/lazarus"
    echo "  2. Set FPCUPDELUXE=/path/to/fpcupdeluxe"
    exit 1
fi

# Convert paths to Windows format if needed (FPC on Windows needs Windows paths)
SCRIPT_DIR_W=$(to_windows_path "$SCRIPT_DIR")
LAZARUSDIR_W=$(to_windows_path "$LAZARUSDIR")
LAZ="$LAZARUSDIR_W/components"

# Source directories to include (use Windows paths for FPC)
SRC_UNITS="-Fu$SCRIPT_DIR_W/protocol -Fu$SCRIPT_DIR_W/serverprotocol -Fu$SCRIPT_DIR_W/standard"

# Lazarus component units (CodeTools, lazutils, jcf2)
LAZ_JCF2="-Fu${LAZ}/jcf2/* -Fu${LAZ}/jcf2 -Fu${LAZ}/jcf2/Settings/* -Fu${LAZ}/jcf2/Parse/* -Fu${LAZ}/jcf2/Process/*"
LAZ_COMPONENT_UNITS="-Fu${LAZ}/codetools -Fu${LAZ}/lazutils ${LAZ_JCF2}"

# Temp directory (cross-platform)
TMPDIR="${TMPDIR:-${TMP:-${TEMP:-/tmp}}}"

# Get target info (os:cpu) for a given target name
# Returns empty string if target is unknown
# Uses case statement for bash 3.x compatibility (macOS default)
get_target_info() {
    case "$1" in
        x86_64-linux)   echo "linux:x86_64" ;;
        aarch64-linux)  echo "linux:aarch64" ;;
        i386-win32)     echo "win32:i386" ;;
        x86_64-win64)   echo "win64:x86_64" ;;
        x86_64-darwin)  echo "darwin:x86_64" ;;
        aarch64-darwin) echo "darwin:aarch64" ;;
        *)              echo "" ;;
    esac
}

SUCCESS_COUNT=0
FAIL_COUNT=0
FAILED_TARGETS=""
USE_SQLITE=0
BUILD_TESTS=0

#=============================================================================
# Build functions
#=============================================================================

# Check if target is native (no cross-compilation needed)
is_native_target() {
    local target_os="$1"
    local target_cpu="$2"

    # Normalize OS names for comparison
    local host_os_normalized="$HOST_OS"
    local target_os_normalized="$target_os"

    # win32/win64 are both Windows
    [[ "$host_os_normalized" == win* ]] && host_os_normalized="windows"
    [[ "$target_os_normalized" == win* ]] && target_os_normalized="windows"

    [ "$host_os_normalized" = "$target_os_normalized" ] && [ "$HOST_CPU" = "$target_cpu" ]
}

# Check if cross-compiler is available
check_cross_compiler() {
    local target_os="$1"
    local target_cpu="$2"

    # Test if fpc can compile for this target
    if "$FPC_CMD" -T"$target_os" -P"$target_cpu" -iV &>/dev/null; then
        return 0
    fi
    return 1
}

build_target() {
    local name="$1"
    local target_info
    target_info="$(get_target_info "$name")"

    if [ -z "$target_info" ]; then
        echo -e "${RED}Unknown target: $name${NC}"
        ((FAIL_COUNT++))
        return 1
    fi

    local os=$(echo "$target_info" | cut -d: -f1)
    local cpu=$(echo "$target_info" | cut -d: -f2)

    local output_dir="$OUTDIR/$name"
    local unit_output_dir="$BUILDDIR/$name/units"
    local exe_name="pasls"

    # Windows executables have .exe extension
    if [[ "$os" == win* ]]; then
        exe_name="pasls.exe"
    fi

    echo -e "${YELLOW}Building: $name (OS=$os, CPU=$cpu)${NC}"

    # Check if this is native or cross-compilation
    if is_native_target "$os" "$cpu"; then
        echo "  Mode: native"
    else
        echo "  Mode: cross-compilation"
        if ! check_cross_compiler "$os" "$cpu"; then
            echo -e "${RED}  SKIPPED: Cross-compiler not available for $os/$cpu${NC}"
            echo "  Install FPC cross-compiler for this target or run on native platform."
            ((FAIL_COUNT++))
            FAILED_TARGETS="$FAILED_TARGETS $name"
            return 1
        fi
    fi

    # Create output directories
    mkdir -p "$output_dir"
    mkdir -p "$unit_output_dir"

    # Build compiler options
    local opts=""

    # Load fpc.cfg if available (required for RTL unit paths)
    # This is how Lazarus does it - explicitly pass @fpc.cfg
    if [ -n "$FPC_CFG" ] && [ -f "$FPC_CFG" ]; then
        opts="@$FPC_CFG"
    fi

    # Target OS and CPU
    opts="$opts -T$os -P$cpu"

    # Cross-compilation RTL unit paths (from FPC_CROSS_UNITS environment)
    if [ -n "$FPC_CROSS_UNITS" ]; then
        opts="$opts -Fu${FPC_CROSS_UNITS}"
        opts="$opts -Fu${FPC_CROSS_UNITS}/*"
    fi

    # Output options (convert to Windows paths for FPC on Windows)
    local unit_output_dir_w=$(to_windows_path "$unit_output_dir")
    local output_dir_w=$(to_windows_path "$output_dir")
    opts="$opts -FU$unit_output_dir_w"       # Unit output directory
    opts="$opts -o$output_dir_w/$exe_name"   # Executable output

    # Optimization for release build
    opts="$opts -O2"                        # Optimization level 2
    opts="$opts -Xs"                        # Strip debug symbols

    # Include paths
    opts="$opts $SRC_UNITS"
    opts="$opts $LAZ_COMPONENT_UNITS"

    # SQLite support
    if [ "$USE_SQLITE" -eq 1 ]; then
        opts="$opts -dUSE_SQLITE"
    fi

    # Verbosity
    opts="$opts -vbr"                       # Brief output (errors, warnings)

    local log_file="$TMPDIR/build_fpc_$$.log"

    # Run compiler
    echo "  Compiler: $FPC_CMD"

    if "$FPC_CMD" $opts "$PROGRAM" > "$log_file" 2>&1; then
        if [ -f "$output_dir/$exe_name" ]; then
            local size
            if [[ "$HOST_OS" == darwin ]]; then
                size=$(stat -f%z "$output_dir/$exe_name" | awk '{printf "%.1fM", $1/1024/1024}')
            else
                size=$(ls -lh "$output_dir/$exe_name" | awk '{print $5}')
            fi
            echo -e "${GREEN}  SUCCESS: $output_dir/$exe_name ($size)${NC}"
            ((SUCCESS_COUNT++))
            rm -f "$log_file"
            return 0
        else
            echo -e "${RED}  FAILED: Output not found at $output_dir/$exe_name${NC}"
            cat "$log_file"
            ((FAIL_COUNT++))
            FAILED_TARGETS="$FAILED_TARGETS $name"
            rm -f "$log_file"
            return 1
        fi
    else
        echo -e "${RED}  FAILED: $name${NC}"
        echo "  Build log:"
        cat "$log_file"
        ((FAIL_COUNT++))
        FAILED_TARGETS="$FAILED_TARGETS $name"
        rm -f "$log_file"
        return 1
    fi
}

print_summary() {
    echo ""
    echo "========================================"
    echo -e "Build Summary: ${GREEN}${SUCCESS_COUNT} succeeded${NC}, ${RED}${FAIL_COUNT} failed${NC}"
    if [ -n "$FAILED_TARGETS" ]; then
        echo -e "${RED}Failed targets:${FAILED_TARGETS}${NC}"
    fi
    echo "Output directory: $OUTDIR/"
    if [ "$USE_SQLITE" -eq 1 ]; then
        echo "SQLite support: ENABLED"
    else
        echo "SQLite support: DISABLED"
    fi
    echo "========================================"
}

# Build testlsp for a specific target
build_tests() {
    local name="$1"
    local target_info
    target_info="$(get_target_info "$name")"

    if [ -z "$target_info" ]; then
        echo -e "${RED}Unknown target for tests: $name${NC}"
        return 1
    fi

    local os=$(echo "$target_info" | cut -d: -f1)
    local cpu=$(echo "$target_info" | cut -d: -f2)

    local output_dir="$BUILDDIR/$name"
    local unit_output_dir="$BUILDDIR/$name/units"
    local exe_name="testlsp"

    if [[ "$os" == win* ]]; then
        exe_name="testlsp.exe"
    fi

    echo -e "${YELLOW}Building tests: $name${NC}"

    # Create output directory
    mkdir -p "$output_dir"
    mkdir -p "$unit_output_dir"

    # Build compiler options
    local opts=""

    # Load fpc.cfg if available
    if [ -n "$FPC_CFG" ] && [ -f "$FPC_CFG" ]; then
        opts="@$FPC_CFG"
    fi

    # Target OS and CPU
    opts="$opts -T$os -P$cpu"

    # Cross-compilation RTL unit paths (from FPC_CROSS_UNITS environment)
    if [ -n "$FPC_CROSS_UNITS" ]; then
        opts="$opts -Fu${FPC_CROSS_UNITS}"
        opts="$opts -Fu${FPC_CROSS_UNITS}/*"
    fi

    # Output options (testlsp goes to build dir, not dist)
    # Convert to Windows paths for FPC on Windows
    local unit_output_dir_w=$(to_windows_path "$unit_output_dir")
    local output_dir_w=$(to_windows_path "$output_dir")
    opts="$opts -FU$unit_output_dir_w"
    opts="$opts -o$output_dir_w/$exe_name"

    # Include paths for tests
    opts="$opts -Fu$SCRIPT_DIR_W/protocol"
    opts="$opts -Fu$SCRIPT_DIR_W/serverprotocol"
    opts="$opts -Fu$SCRIPT_DIR_W/tests"
    opts="$opts $LAZ_COMPONENT_UNITS"

    # SQLite support (same as main build)
    if [ "$USE_SQLITE" -eq 1 ]; then
        opts="$opts -dUSE_SQLITE"
    fi

    # Verbosity
    opts="$opts -vbr"

    local log_file="$TMPDIR/build_tests_$$.log"
    local test_src="$SCRIPT_DIR_W/tests/testlsp.lpr"

    if "$FPC_CMD" $opts "$test_src" > "$log_file" 2>&1; then
        if [ -f "$output_dir/$exe_name" ]; then
            echo -e "${GREEN}  Tests built: $output_dir/$exe_name${NC}"
            rm -f "$log_file"
            return 0
        else
            echo -e "${RED}  FAILED: Test output not found${NC}"
            cat "$log_file"
            rm -f "$log_file"
            return 1
        fi
    else
        echo -e "${RED}  FAILED: testlsp build${NC}"
        tail -30 "$log_file"
        rm -f "$log_file"
        return 1
    fi
}

# Run tests for a specific target (native only)
run_tests() {
    local name="$1"
    local target_info
    target_info="$(get_target_info "$name")"
    local os=$(echo "$target_info" | cut -d: -f1)
    local cpu=$(echo "$target_info" | cut -d: -f2)

    # Tests can only run on native platform
    if ! is_native_target "$os" "$cpu"; then
        return 0
    fi

    local output_dir="$BUILDDIR/$name"
    local exe_name="testlsp"
    [[ "$os" == win* ]] && exe_name="testlsp.exe"

    if [ ! -f "$output_dir/$exe_name" ]; then
        echo -e "${RED}  Test executable not found: $output_dir/$exe_name${NC}"
        return 1
    fi

    echo -e "${YELLOW}Running tests: $name${NC}"
    if "$output_dir/$exe_name"; then
        echo -e "${GREEN}  Tests PASSED${NC}"
        return 0
    else
        echo -e "${RED}  Tests FAILED${NC}"
        return 1
    fi
}

# Check SQLite development libraries when --sqlite is used
check_sqlite_dev() {
    case "$HOST_OS" in
        linux)
            # Check for sqlite3.h header file
            if [ -f "/usr/include/sqlite3.h" ]; then
                return 0
            fi
            # Check via pkg-config
            if command -v pkg-config &>/dev/null && pkg-config --exists sqlite3 2>/dev/null; then
                return 0
            fi
            echo -e "${RED}Error: SQLite development libraries not found.${NC}"
            echo ""
            echo "Please install SQLite development package:"
            echo "  Debian/Ubuntu: sudo apt-get install libsqlite3-dev"
            echo "  RHEL/Fedora:   sudo dnf install sqlite-devel"
            echo "  Arch Linux:    sudo pacman -S sqlite"
            echo ""
            return 1
            ;;
        darwin)
            # macOS usually has SQLite pre-installed
            if [ -f "/usr/include/sqlite3.h" ] || [ -f "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sqlite3.h" ]; then
                return 0
            fi
            # Check via pkg-config (Homebrew)
            if command -v pkg-config &>/dev/null && pkg-config --exists sqlite3 2>/dev/null; then
                return 0
            fi
            echo -e "${RED}Error: SQLite development libraries not found.${NC}"
            echo ""
            echo "Please install SQLite via Homebrew:"
            echo "  brew install sqlite"
            echo ""
            return 1
            ;;
        win32|win64)
            # On Windows/MSYS2, check common locations
            if [ -f "/mingw64/include/sqlite3.h" ] || [ -f "/mingw32/include/sqlite3.h" ] || [ -f "/usr/include/sqlite3.h" ]; then
                return 0
            fi
            # Skip check on Windows - FPC may have built-in SQLite support
            return 0
            ;;
        *)
            # Unknown platform, skip check
            return 0
            ;;
    esac
}

#=============================================================================
# Parse arguments
#=============================================================================
TARGETS_TO_BUILD=""
while [ $# -gt 0 ]; do
    case "$1" in
        --sqlite)
            USE_SQLITE=1
            shift
            ;;
        --tests)
            BUILD_TESTS=1
            shift
            ;;
        --help|-h)
            # Already handled above, but keep for completeness
            show_help
            exit 0
            ;;
        all)
            TARGETS_TO_BUILD="$LINUX_TARGETS $WINDOWS_TARGETS $DARWIN_TARGETS"
            shift
            ;;
        linux)
            TARGETS_TO_BUILD="$TARGETS_TO_BUILD $LINUX_TARGETS"
            shift
            ;;
        windows|win)
            TARGETS_TO_BUILD="$TARGETS_TO_BUILD $WINDOWS_TARGETS"
            shift
            ;;
        darwin|macos|osx)
            TARGETS_TO_BUILD="$TARGETS_TO_BUILD $DARWIN_TARGETS"
            shift
            ;;
        *)
            TARGETS_TO_BUILD="$TARGETS_TO_BUILD $1"
            shift
            ;;
    esac
done

# Default: build for current platform only
if [ -z "$TARGETS_TO_BUILD" ]; then
    case "$HOST_PLATFORM" in
        x86_64-linux)   TARGETS_TO_BUILD="x86_64-linux" ;;
        aarch64-linux)  TARGETS_TO_BUILD="aarch64-linux" ;;
        i386-win32)     TARGETS_TO_BUILD="i386-win32" ;;
        x86_64-win64)   TARGETS_TO_BUILD="x86_64-win64" ;;
        x86_64-darwin)  TARGETS_TO_BUILD="x86_64-darwin" ;;
        aarch64-darwin) TARGETS_TO_BUILD="aarch64-darwin" ;;
        *)
            echo -e "${RED}Error: Unknown host platform: $HOST_PLATFORM${NC}"
            echo "Please specify target explicitly."
            exit 1
            ;;
    esac
    echo "No target specified, building for current platform: $TARGETS_TO_BUILD"
fi

#=============================================================================
# Main
#=============================================================================
echo "Pascal Language Server - FPC Direct Build"
echo "=========================================="
echo "Host platform: $HOST_PLATFORM"
echo "FPC: $FPC_CMD"
if [ -n "$FPC_CFG" ]; then
    echo "FPC config: $FPC_CFG"
else
    echo "FPC config: (not found - may fail to locate RTL units)"
fi
echo "Lazarus: $LAZARUSDIR"
if [ "$USE_SQLITE" -eq 1 ]; then
    echo "SQLite support: ENABLED"
    # Check for SQLite development libraries
    if ! check_sqlite_dev; then
        exit 1
    fi
else
    echo "SQLite support: DISABLED (default)"
fi
echo ""

# Build targets
for target in $TARGETS_TO_BUILD; do
    build_target "$target"
done

# Build tests if requested (tests only built, not run - use testlsp directly to run)
if [ "$BUILD_TESTS" -eq 1 ]; then
    echo ""
    echo "Building tests..."
    for target in $TARGETS_TO_BUILD; do
        build_tests "$target"
    done
fi

print_summary
exit $FAIL_COUNT
