#version 330 core

in vec2 UV;

out vec3 color;


// Values that stay constant for the whole mesh.
uniform sampler2D sampler;

void main(){

  // Output color = color of the texture at the specified UV
  color = texture( sampler, UV ).rgb;
}
