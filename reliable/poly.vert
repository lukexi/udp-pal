#version 330 core

uniform mat4 uViewProjection;
uniform mat4 uModel;
uniform mat4 uInverseModel;

// Used by font rendering
uniform float uXOffset;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec3 aTangent;
in      vec2 aUV;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;

void main() {
    // Apply all matrix transformations to vert
    vec4 finalPosition = uModel * vec4(aPosition.x + uXOffset, aPosition.y, aPosition.z, 1.0);
    gl_Position = uViewProjection * finalPosition;
    
    // Pass some variables to the fragment shader
    vPosition = vec3(finalPosition);
    vNormal   = (uInverseModel * vec4(aNormal, 1.)).xyz;
    vUV       = aUV;
}
