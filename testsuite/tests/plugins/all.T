def f(name, opts):
    if (ghc_with_interpreter == 0):
        opts.skip = 1

setTestOpts(f)
setTestOpts(when(compiler_lt('ghc', '7.1'), skip))

test('plugins01',
     [pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins01'),
      clean_cmd('$MAKE -s --no-print-directory -C simple-plugin clean.plugins01')],
     run_command,
     ['$MAKE -s --no-print-directory plugins01'])
test('plugins02',
     [pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins02'),
      clean_cmd('$MAKE -s --no-print-directory -C simple-plugin clean.plugins02')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins02/local.package.conf -fplugin Simple.BadlyTypedPlugin -package simple-plugin ' + config.ghci_way_flags])

test('plugins03',
     [pre_cmd('$MAKE -s --no-print-directory -C simple-plugin package.plugins03'),
      clean_cmd('$MAKE -s --no-print-directory -C simple-plugin clean.plugins03')],
     compile_fail,
     ['-package-db simple-plugin/pkg.plugins03/local.package.conf -fplugin Simple.NonExistantPlugin -package simple-plugin'])

test('plugins04',
     extra_clean(['HomePackagePlugin.hi', 'HomePackagePlugin.o']),
     multimod_compile_fail,
     ['plugins04', '-package ghc -fplugin HomePackagePlugin'])

test('plugins05',
     [ extra_clean(['HomePackagePlugin.hi', 'HomePackagePlugin.o']),
       only_ways([config.ghc_plugin_way]) ],
     multimod_compile_and_run, ['plugins05', '-package ghc'])

test('plugins06',
     [ extra_clean(['LinkerTicklingPlugin.hi', 'LinkerTicklingPlugin.o']),
       unless(have_dynamic(),skip),
       only_ways([config.ghc_plugin_way]) ],
     multimod_compile_and_run, ['plugins06', '-package ghc'])

test('plugins07',
     [pre_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin package.plugins07'),
      clean_cmd('$MAKE -s --no-print-directory -C rule-defining-plugin clean.plugins07'),
      expect_broken(10420)],
     run_command,
     ['$MAKE -s --no-print-directory plugins07'])
