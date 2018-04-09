float hypot(vec2 p) {
    return sqrt(p.x*p.x+p.y*p.y);
}

float pi = 3.1415926;

float cross2(vec2 u, vec2 v) {
    return u.x*v.y-u.y*v.x;
}

vec2 path(float t) {
    float theta = 2.0*pi*(t-0.5);
    float alpha = 0.2*iTime;
    mat2 m = mat2(cos(alpha), -sin(alpha), sin(alpha), cos(alpha));

    //vec2 xy = vec2(0.75*cos(theta), sin(theta));
    //vec2 xy = vec2(theta, 1.5*sqrt(theta*theta*theta-theta*theta)-1.0);
    //return vec2(-theta*theta, theta);
    vec2 xy = vec2(-cos(2.0*theta+2.0*sin(3.0*theta)), sin(theta+pi/3.0+cos(5.0*theta+0.5*iTime)));
    //vec2 xy = vec2(cos(theta)*cos(theta+sin(2.5*theta+pi/4.0+iTime))*cos(theta), sin(theta+2.0*t)*sin(theta)*sin(theta));

    return 0.75*m*xy;
}

// float l(vec2 xy, float theta) {
//     return -path(theta).x+hypot(xy-path(theta));
// }

float gain = 0.5;
//float wavelength = 0.03;

float integrate(vec2 xy) {
    float tx = 0.0;
    float ty = 0.0;
    int n = 300;
    vec2 last_p = path(0.0);

    float wavelength = 0.01;//+0.1*iMouse.x/iResolution.x;

    for (int i = 0; i < n; ++i) {
        float t = float(i)/float(n);
        vec2 p = path(t);
        float d = hypot(xy-p);

        float dt = p.y-last_p.y;

        if (dt < 0.0 && cross2(p-last_p, xy-p) > 0.0) {
            float path_length = d-p.x;
            float s = 2.0*pi/wavelength*path_length;
            tx += cos(s)*dt/d;
            ty += sin(s)*dt/d;
        }

//        path_length = d+p.x;
//        s = -2.0*pi/wavelength*path_length;
//        tx += cos(s)*dt/d;
//        ty += sin(s)*dt/d;

        last_p = p;
    }
    return hypot(vec2(tx, ty));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 center = vec2(0.5*iResolution.x, 0.5*iResolution.y);
    vec2 uv = fragCoord.xy-center;
    uv = 2.0*uv/iResolution.y;
    if (false && length(uv) >= 1.0) {
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }
    float c = gain*integrate(uv);
    fragColor = vec4(0.8*c, c, 0.5*c, 1.0);
}
