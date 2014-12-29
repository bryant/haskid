import Distribution.Simple
    ( defaultMainWithHooks
    , simpleUserHooks
    , UserHooks(preBuild, preClean, buildHook)
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

main = defaultMainWithHooks simpleUserHooks { buildHook = try_ffi_build }


get_bench_info :: PackageDescription
               -> Maybe (BuildInfo, Executable, [Executable])
get_bench_info pkgdesc@(PackageDescription {executables=execs}) =
    case filter is_bench execs of
        [] -> Nothing
        (exe:_) -> let binfo = buildInfo exe in
            if buildable binfo then Just (binfo, exe, filter not_bench execs)
                               else Nothing
    where
    is_bench = (== "benchmarks") . exeName
    not_bench = (/= "benchmarks") . exeName

try_ffi_build pkgdesc locinfo hooks bflags = case get_bench_info pkgdesc of
    Nothing -> buildHook simpleUserHooks pkgdesc locinfo hooks bflags
    Just (binfo, exe, nexe) -> do
        ecode <- system "make -C bench hashidsffi.o hashidsxx/hashids.o"
        when (ecode /= ExitSuccess) $ die "FFI build failed."
        let binfo' = binfo { ldOptions = ldOptions binfo
                                       ++ [ "bench/hashidsffi.o"
                                          , "bench/hashidsxx/hashids.o"] }
        let pkgdesc' = pkgdesc { executables =
                                 exe { buildInfo = binfo' } : nexe }
        buildHook simpleUserHooks pkgdesc' locinfo hooks bflags

bench_prebuild args buildflags = do
    print args
    print buildflags

bench_preclean args buildflags = print args >> print buildflags
