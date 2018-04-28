float turbulence(vec3 x) {
    return noise1(x)+0.5*noise1(2.0*x)+0.25*noise1(4.0*x)+
            0.125*noise1(8.0*x)+0.0625*noise1(16.0*x);
}

float f(vec3 x) {
    float value = min(max(max(-1.0*x.z-1.0, x.x+0.1*x.y+0.01*x.z+0.2), x.y-0.5), x.y+0.5);
    if (value < 0.05) {
        value += 0.05*turbulence(4.0*x);
    }
    return value;
}

float eps = 0.01;
float lambda = 2.0;

vec3 march(vec3 p, vec3 d) {
    float c;
    c = f(p);
    if (c < 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    for (int i = 0; i < 25; ++i) {
        float step = max(0.001, lambda*c);
        p = p+step*d;
        c = f(p);
        if (c < 0) {
            float ex, ey, ez;
            ex = f(p+vec3(eps, 0.0, 0.0));
            ey = f(p+vec3(0.0, eps, 0.0));
            ez = f(p+vec3(0.0, 0.0, eps));
            vec3 n = vec3(ex-c, ey-c, ez-c)/eps;
            n = normalize(n);
            float l = 0.25+0.75*clamp(dot(n, vec3(1.0, 1.0, -1.0))/sqrt(3.0), 0.0, 10.0);
            return vec3(l);
        }
    }
    return vec3(0.1, 0.1, 0.3);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy-0.5*iResolution.xy;
    uv = 2.0*uv/iResolution.y;
    vec2 xy = iMouse/iResolution;

    vec3 p = vec3(0.0, 0.0, -2.0)+0.1*vec3(xy.x, xy.y, 0.0);;
    vec3 d = vec3(0.5*uv, 1.0);
    vec3 color = march(p, d);
    fragColor = vec4(color, 1.0);
}
