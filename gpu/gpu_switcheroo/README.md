# GPU Switcheroo

Utility to switch between intel GPU only (disables the nvidia GPU) and
optimus (intel+nvidia GPU).

Ideally
[nvidia-prime-select](https://github.com/wildtruc/nvidia-prime-select)
works perfectly but this is not the case. It is able to switch to the
intel GPU only but when attempting to switch to the nvidia GPU only,
it gets stuck at boot. This is likely caused due to GDM (it is also
mentioned in the `nvidia-prime-select` README file) but no fix
works. Since `optimus` works well and switching to intel GPU only also
works, it makes sense to have some utility to help set this up well.

It works by installing `nvidia-prime-select` (and selecting the intel
GPU through this) when selecting the intel GPU in
`gpu_switcheroo`. When selecting `optimus` (intel+nvidia GPU), it
uninstalls `nvidia-prime-select` thus enabling both the GPUs again and
`optimus` technology can be leveraged.

## Note

This requires access to the source code of `nvidia-prime-select`,
either download the source code or clone the repository. Set the path
using `gpu_switcheroo --set-nvidia-prime-select-path
<SET_NVIDIA_PRIME_SELECT_PATH> intel`.
