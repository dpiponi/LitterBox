# LitterBox
A wee Haskell app to provide a local sandbox a bit like ShaderToy (but lacking many features).

Launch it with `./LitterBox -d <dirname>`
  
If you omit `<dirname>` it assumes the current directory
  
It compiles and renders with `litterbox.vert` and `litterbox.frag`.
Only `litterbox.frag` is intended to be user-editable.

Supports iMouse, iResolution and iTime. Uses GLSL rather than GLSL ES.

It watches the chosen directory and will recompile the shaders if it notices modifications.
If the compiler complains it'll keep rendering using the previously working shader.


![Example image](https://raw.githubusercontent.com/dpiponi/LitterBox/master/Untitled.png)
