# Pastel3d
A 3D Renderer Project for Commodore 64

Pastel3d is an experimental 3D rendering program for Commodore 64.

Features:
- Works by projecting rays from the surface of geometrical objects without using meshes.
- Based on 24-HSL color space
- Uses dithering to match the color
- Supports Lores (MCM), Hires and Laced modes
- Utilized 32-bit floating point module

Important:
- You need KickAssembler to assemble the code.
- The file "settings.inc" stores the scene information.
- You can use "display.prg" to display images.
- A C64 emulator with turbo mode is recommended (Rendering process is too slow).

Future work:
- Provide modularity by separating color conversion module from rendering module.
- FLI, IFLI modes
- Fixed point artihmetic for speed-up
