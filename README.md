haskid
======

Screaming fast [Hashids](http://hashids.org) in Haskell.

### Build

```bash
$ cabal sandbox init  # create a sandbox as appropriate
$ cabal install --only-dep  # install haskid's dependencies
$ cabal install  # build and install haskid
```

### Usage

```bash
$ cd $PROJECT_ROOT
$ cabal exec ghci
09:25:15 ~/dev/haskid> cabal exec ghci
Prelude> import Data.HaskID
Prelude Data.HaskID> let Right options = init_haskid opts { opt_salt = "this is my salt" }
Prelude Data.HaskID> encode options [1, 2, 3]
"laHquq"
Prelude Data.HaskID> decode options (encode options [1, 2, 3])
[1,2,3]
Prelude Data.HaskID>
```

### Benchmarks

Build and run benchmarks which pit haskid against the C++11 implementation,
[hashidsxx](https://github.com/schoentoon/hashidsxx):

```bash
$ cd $PROJECT_ROOT
$ cabal configure --flags=-bench  # configure with benchmarks enabled
$ cabal build
$ ./dist/build/benchmarks/benchmarks --output benchmarks.html
```

Existing benchmarks results are also available
[here](https://bryant.github.io/haskid/benchmarks.html),
which were run under a twelve-core i7-3930K:

```bash
$ tail -n 26 /proc/cpuinfo
processor       : 11
vendor_id       : GenuineIntel
cpu family      : 6
model           : 45
model name      : Intel(R) Core(TM) i7-3930K CPU @ 3.20GHz
stepping        : 7
microcode       : 0x710
cpu MHz         : 3200.005
cache size      : 12288 KB
physical id     : 0
siblings        : 12
core id         : 5
cpu cores       : 6
apicid          : 11
initial apicid  : 11
fpu             : yes
fpu_exception   : yes
cpuid level     : 13
wp              : yes
flags           : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx pdpe1gb rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm pcid dca sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer aes xsave avx lahf_lm arat epb xsaveopt pln pts dtherm tpr_shadow vnmi flexpriority ept vpid
bogomips        : 6400.04
clflush size    : 64
cache_alignment : 64
address sizes   : 46 bits physical, 48 bits virtual
power management:
```
