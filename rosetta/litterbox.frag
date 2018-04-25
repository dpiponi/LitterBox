float f(vec3 x) {
    return noise1(30.0*x);//1.0+x.z;
}

float eps = 0.001;

vec3 march(vec3 p, vec3 d) {
    if (f(p) < 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    for (int i = 0; i < 100; ++i) {
        p = p+0.01*d;
        float c = f(p);
        if (c < 0) {
            float ex, ey, ez;
            ex = f(p+vec3(eps, 0.0, 0.0));
            ey = f(p+vec3(0.0, eps, 0.0));
            ez = f(p+vec3(0.0, 0.0, eps));
            vec3 n = vec3(ex-c, ey-c, ez-c)/eps;
            n = normalize(n);
            float l = 0.5+0.5*clamp(dot(n, vec3(1.0, 1.0, -1.0))/sqrt(3.0), 0.0, 10.0);
            return vec3(l);
        }
    }
    return vec3(0.0, 0.0, 0.0);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy-0.5*iResolution.xy;
    uv = 2.0*uv/iResolution.y;

    vec3 p = vec3(0.0, 0.0, -2.0);
    vec3 d = vec3(0.5*uv, 1.0);
    vec3 color = march(p, d);
    fragColor = vec4(color, 1.0);
}
