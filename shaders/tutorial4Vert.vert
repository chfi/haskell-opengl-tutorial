#version 330 core

in vec3 aPosition;
in vec4 aColor;

out vec4 vColor;

uniform mat4 mvp;

void main()
{
  gl_Position = mvp * vec4(aPosition.x, aPosition.y, aPosition.z, 1.0);
  vColor = aColor;
  // vColor = vec4(0.5f, 0.f, 0.f, 0.f);
}
