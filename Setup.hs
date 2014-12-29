import Distribution.Simple
    ( defaultMainWithHooks
    , simpleUserHooks
    , UserHooks(buildHook, cleanHook)
    )
import Distribution.PackageDescription
    ( PackageDescription(..)
    , Executable(..)
    , BuildInfo(..)
    )
import Distribution.Simple.Utils (die)
import System.Process (system)
import System.Exit (ExitCode(ExitSuccess))
import Control.Monad (when)

main = defaultMainWithHooks simpleUserHooks
    { buildHook = try_ffi_build
    , cleanHook = clean_ffi_build
    }

bench_enabled :: PackageDescription -> Bool
bench_enabled pkgdesc = case filter is_bench $ executables pkgdesc of
    Executable {buildInfo=BuildInfo {buildable=True}} : _ -> True
    _ -> False
    where is_bench = (== "benchmarks") . exeName

try_ffi_build pkgdesc locinfo hooks bflags = do
    when (bench_enabled pkgdesc) $ do
        putStrLn "Invoking make -C bench hashidsffi.o hashidsxx/hashids.o"
        ecode <- system "make -C bench hashidsffi.o hashidsxx/hashids.o"
        when (ecode /= ExitSuccess) $ die "FFI build failed."
    buildHook simpleUserHooks pkgdesc locinfo hooks bflags

clean_ffi_build pkgdesc _ hooks cflags = do
    putStrLn "Cleaning FFI bench area"
    system "make -C bench clean"
    cleanHook simpleUserHooks pkgdesc () hooks cflags
