To switch between intel GPU only and intel+nvidia GPU, use
`gpu_switcheroo`.

Compile and install (`cargo deb` is the easiest way).

`gpu_switcheroo` needs the path to the local download/clone of
https://github.com/wildtruc/nvidia-prime-select . Set this using
`gpu_switcheroo --set-nvidia-prime-select-path intel`.
