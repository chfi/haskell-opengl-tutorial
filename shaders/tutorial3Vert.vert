#version 330 core

in vec3 aPosition;

uniform mat4 mvp;

void main()
{
  gl_Position = mvp * vec4(aPosition.x, aPosition.y, aPosition.z, 1.0);
}
