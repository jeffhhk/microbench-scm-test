# Synopsis

Benchmark code for generators, streams and yield-style coroutines.

Runs across different Scheme implementations, currently:

- Chez
- Gambit
- Racket

A few tests pertaining to implementation-specific features (e.g. FFI, one-shot continuations) are included.

# Library

The measurement routines reside in a separate repository [microbench-scm](https://github.com/jeffhhk/microbench-scm) , a [microbenchmarking](https://stackoverflow.com/questions/2842695/what-is-microbenchmarking) implementation, integrated as a git subtree.  No additional operations besides git clone are required to use the library.

# Measurement method

We measure iteration duration using a median of a mean.

Each test measures a loop of length _n_ repeated for _k_ trials.  The wall clock measures the duration of each loop.  We estimate mean duration per iteration by dividing the loop duration by _n_.

We observe the _k_ trials of the loop and quantile them.

The measurement function dbench takes the loop function g as an argument.  There are two optional function parameters, gen and f.  Function gen allows data generation to be performed separately from the measurement being taken.  Function f provides a "placebo" behavior to contain a loop without any contents.  The measurement actually taken is the time of g minus the time of f.  Each trial subtracts these two numbers, in a similar fashion to a differential amplifier.

# Output format

The _p_=0.5 measurement is the median trial.  The other _p_ keys are for other quantiles, 0.95, 0.995, etc, until the end of the k trials.  The _p_=1.0 is the longest trial.

The key _ns_ indicates duration in nanoseconds.  Each test output is named and formatted like so:

    ("bvcopy-loop" (((p 0.5) (ns 2.39978)) ((p 0.95) (ns 2.46683)) ((p 0.995) (ns 3.0597950000000003)) ((p 0.9996666666666667) (ns 3.42437)) ((p 1.0) (ns 3.5668450000000003))))


# Example output

With Chez 9.5, Gambit 4.9.4 (gcc 9.4.0), and Racket 8.6, on Ubuntu 20 on [AMD Ryzen 7 4800H](https://www.amd.com/en/products/apu/amd-ryzen-7-4800h).

bash ./run-on-chez.sh

    ("bvcopy-loop" (((p 0.5) (ns 2.39978)) ((p 0.95) (ns 2.46683)) ((p 0.995) (ns 3.0597950000000003)) ((p 0.9996666666666667) (ns 3.42437)) ((p 1.0) (ns 3.5668450000000003))))
    ("bvcopy-loop-noop" (((p 0.5) (ns 0.707505)) ((p 0.95) (ns 0.73684)) ((p 0.995) (ns 1.63536)) ((p 0.9996666666666667) (ns 1.8700350000000001)) ((p 1.0) (ns 2.03032))))
    ("bvcopy-fxloop-noop" (((p 0.5) (ns 0.47423)) ((p 0.95) (ns 0.49169)) ((p 0.995) (ns 0.5479200000000001)) ((p 0.9996666666666667) (ns 0.623)) ((p 1.0) (ns 0.7054050000000001))))
    ("bvcopy-fxloop-read" (((p 0.5) (ns 0.474225)) ((p 0.95) (ns 0.49169)) ((p 0.995) (ns 0.54931)) ((p 0.9996666666666667) (ns 0.7218249999999999)) ((p 1.0) (ns 0.724265))))
    ("bvcopy-fxloop-write" (((p 0.5) (ns 0.995255)) ((p 0.95) (ns 1.02354)) ((p 0.995) (ns 1.068935)) ((p 0.9996666666666667) (ns 1.0933899999999999)) ((p 1.0) (ns 1.113285))))
    ("bvcopy-fxloop-all" (((p 0.5) (ns 0.993855)) ((p 0.95) (ns 1.025635)) ((p 0.995) (ns 1.064395)) ((p 0.9996666666666667) (ns 1.07662)) ((p 1.0) (ns 1.082905))))
    ("consume generator" (((p 0.5) (ns 2.1176)) ((p 0.95) (ns 2.20704)) ((p 0.995) (ns 2.57576)) ((p 0.9996666666666667) (ns 3.8106)) ((p 1.0) (ns 11.772639999999999))))
    ("consume stream" (((p 0.5) (ns 5.76616)) ((p 0.95) (ns 7.2636)) ((p 0.995) (ns 10.04056)) ((p 0.9996666666666667) (ns 29.018079999999998)) ((p 1.0) (ns 31.5492))))
    ("consume stream 2" (((p 0.5) (ns 5.78016)) ((p 0.95) (ns 7.18536)) ((p 0.995) (ns 11.87324)) ((p 0.9996666666666667) (ns 28.381120000000003)) ((p 1.0) (ns 30.764200000000002))))
    ("consume coroutine" (((p 0.5) (ns 48.41196)) ((p 0.95) (ns 52.47676)) ((p 0.995) (ns 99.74607999999999)) ((p 0.9996666666666667) (ns 106.52916)) ((p 1.0) (ns 106.64376))))
    ("consume coroutine3" (((p 0.5) (ns 40.491839999999996)) ((p 0.95) (ns 47.5878)) ((p 0.995) (ns 102.65156)) ((p 0.9996666666666667) (ns 110.54092)) ((p 1.0) (ns 114.50796))))
    ("bvcopy" (((p 0.5) (ns 0.01711)) ((p 0.95) (ns 0.020955)) ((p 0.995) (ns 0.033525)) ((p 0.9996666666666667) (ns 0.090445)) ((p 1.0) (ns 0.100925))))
    ("bvcopy-fxloop-read64" (((p 0.5) (ns 0.201845)) ((p 0.95) (ns 0.209525)) ((p 0.995) (ns 0.233625)) ((p 0.9996666666666667) (ns 0.3548)) ((p 1.0) (ns 0.37959))))
    ("bvcopy-fxloop-write64" (((p 0.5) (ns 0.21477)) ((p 0.95) (ns 0.232925)) ((p 0.995) (ns 0.276575)) ((p 0.9996666666666667) (ns 0.34083)) ((p 1.0) (ns 0.38064))))
    ("bytevector-copy!" (((p 0.5) (ns 0.01711)) ((p 0.95) (ns 0.019555000000000003)) ((p 0.995) (ns 0.032479999999999995)) ((p 0.9996666666666667) (ns 0.06146000000000001)) ((p 1.0) (ns 0.06669499999999999))))
    ("bytevector-copy2!" (((p 0.5) (ns 0.018505)) ((p 0.95) (ns 0.0213)) ((p 0.995) (ns 0.03702)) ((p 0.9996666666666667) (ns 0.13794499999999998)) ((p 1.0) (ns 0.157495))))
    ("vector-copy!" (((p 0.5) (ns 0.13407999999999998)) ((p 0.95) (ns 0.15367999999999998)) ((p 0.995) (ns 0.24864000000000003)) ((p 0.9996666666666667) (ns 0.58948)) ((p 1.0) (ns 1.2628))))
    ("consume coroutine4" (((p 0.5) (ns 30.311600000000002)) ((p 0.95) (ns 40.25992)) ((p 0.995) (ns 68.97916000000001)) ((p 0.9996666666666667) (ns 86.57943999999999)) ((p 1.0) (ns 123.40032000000001))))

bash ./run-on-racket.sh

    ("bvcopy-loop" (((p 0.5) (ns 3.541355)) ((p 0.95) (ns 3.6052600000000004)) ((p 0.995) (ns 3.735174999998808)) ((p 0.9996666666666667) (ns 10.98725)) ((p 1.0) (ns 15.383125))))
    ("bvcopy-loop-noop" (((p 0.5) (ns 0.720775)) ((p 0.95) (ns 0.7536)) ((p 0.995) (ns 1.2980150000023842)) ((p 0.9996666666666667) (ns 1.9077500000047682)) ((p 1.0) (ns 33.616524999997615))))
    ("bvcopy-fxloop-noop" (((p 0.5) (ns 0.485755)) ((p 0.95) (ns 0.50356)) ((p 0.995) (ns 0.573055)) ((p 0.9996666666666667) (ns 0.97325)) ((p 1.0) (ns 2.16616))))
    ("bvcopy-fxloop-read" (((p 0.5) (ns 0.95335)) ((p 0.95) (ns 0.9830300000000001)) ((p 0.995) (ns 1.051475)) ((p 0.9996666666666667) (ns 1.2456399999999999)) ((p 1.0) (ns 2.26394))))
    ("bvcopy-fxloop-write" (((p 0.5) (ns 2.243685)) ((p 0.95) (ns 2.3034000000000003)) ((p 0.995) (ns 2.362065)) ((p 0.9996666666666667) (ns 3.53891)) ((p 1.0) (ns 3.55323))))
    ("bvcopy-fxloop-all" (((p 0.5) (ns 2.96341)) ((p 0.95) (ns 3.05595)) ((p 0.995) (ns 4.246770000000001)) ((p 0.9996666666666667) (ns 4.301945)) ((p 1.0) (ns 60.241485))))
    ("consume generator" (((p 0.5) (ns 2.22656)) ((p 0.95) (ns 2.3635200000000003)) ((p 0.995) (ns 2.75184)) ((p 0.9996666666666667) (ns 3.1512800000000003)) ((p 1.0) (ns 3.16524))))
    ("consume stream" (((p 0.5) (ns 6.44224)) ((p 0.95) (ns 8.70516)) ((p 0.995) (ns 15.60836)) ((p 0.9996666666666667) (ns 29.965159999999997)) ((p 1.0) (ns 723.41036))))
    ("consume stream 2" (((p 0.5) (ns 6.106999999961853)) ((p 0.95) (ns 8.305639999999999)) ((p 0.995) (ns 10.093599999999999)) ((p 0.9996666666666667) (ns 23.28544)) ((p 1.0) (ns 23.53408))))
    ("consume coroutine" (((p 0.5) (ns 182.5232)) ((p 0.95) (ns 188.32292)) ((p 0.995) (ns 240.78012)) ((p 0.9996666666666667) (ns 401.99584)) ((p 1.0) (ns 406.8792))))
    0
    ("consume coroutine3" (((p 0.5) (ns 114.54988)) ((p 0.95) (ns 124.16572000000001)) ((p 0.995) (ns 197.97512)) ((p 0.9996666666666667) (ns 305.57703999999995)) ((p 1.0) (ns 1240.72988))))
    ("consume enumerator->s" (((p 0.5) (ns 321.98724)) ((p 0.95) (ns 350.37116)) ((p 0.995) (ns 485.6976800001526)) ((p 0.9996666666666667) (ns 777.6834)) ((p 1.0) (ns 1493.46136))))

bash ./run-on-gambit.sh

    . . .
    ("bvcopy-loop" (((p .5) (ns .7021427154541016)) ((p .95) (ns .7498264312744141)) ((p .995) (ns 1.1658668518066406)) ((p .9996666666666667) (ns 2.1779537200927734)) ((p 1.) (ns 2.3746490478515625))))
    ("bvcopy-loop-noop" (((p .5) (ns .3898143768310547)) ((p .95) (ns .4100799560546875)) ((p .995) (ns .6234645843505859)) ((p .9996666666666667) (ns .6532669067382812)) ((p 1.) (ns .6866455078125))))
    ("bvcopy-fxloop-noop" (((p .5) (ns .23365020751953125)) ((p .95) (ns .24557113647460938)) ((p .995) (ns .2932548522949219)) ((p .9996666666666667) (ns .3457069396972656)) ((p 1.) (ns .42557716369628906))))
    ("bvcopy-fxloop-read" (((p .5) (ns .23365020751953125)) ((p .95) (ns .24318695068359375)) ((p .995) (ns .29921531677246094)) ((p .9996666666666667) (ns .33974647521972656)) ((p 1.) (ns .3647804260253906))))
    ("bvcopy-fxloop-write" (((p .5) (ns .3123283386230469)) ((p .95) (ns .3314018249511719)) ((p .995) (ns .9572505950927734)) ((p .9996666666666667) (ns 1.1038780212402344)) ((p 1.) (ns 1.1897087097167969))))
    ("bvcopy-fxloop-all" (((p .5) (ns .5257129669189453)) ((p .95) (ns .5519390106201172)) ((p .995) (ns .7307529449462891)) ((p .9996666666666667) (ns .9131431579589844)) ((p 1.) (ns 1.049041748046875))))
    ("consume generator" (((p .5) (ns 3.2711029052734375)) ((p .95) (ns 3.414154052734375)) ((p .995) (ns 3.9386749267578125)) ((p .9996666666666667) (ns 4.3201446533203125)) ((p 1.) (ns 4.3773651123046875))))
    ("consume stream" (((p .5) (ns 8.81195068359375)) ((p .95) (ns 11.320114135742188)) ((p .995) (ns 22.563934326171875)) ((p .9996666666666667) (ns 34.70420837402344)) ((p 1.) (ns 36.02027893066406))))
    ("consume stream 2" (((p .5) (ns 8.697509765625)) ((p .95) (ns 10.690689086914062)) ((p .995) (ns 15.001296997070312)) ((p .9996666666666667) (ns 23.49853515625)) ((p 1.) (ns 25.682449340820312))))
    ("consume coroutine" (((p .5) (ns 157.66143798828125)) ((p .95) (ns 170.06874084472656)) ((p .995) (ns 252.2754669189453)) ((p .9996666666666667) (ns 408.8592529296875)) ((p 1.) (ns 486.9651794433594))))
    ("consume coroutine3" (((p .5) (ns 112.25700378417969)) ((p .95) (ns 128.56483459472656)) ((p .995) (ns 198.3642578125)) ((p .9996666666666667) (ns 266.80946350097656)) ((p 1.) (ns 271.1963653564453))))

