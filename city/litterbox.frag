float hash(vec3 p)  // replace this by something better
{
    p  = fract( p*0.3183099+.1 );
    p *= 17.0;
    return fract( p.x*p.y*p.z*(p.x+p.y+p.z) );
}

float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
    
    return mix(mix(mix( hash(p+vec3(0,0,0)), 
                        hash(p+vec3(1,0,0)),f.x),
                   mix( hash(p+vec3(0,1,0)), 
                        hash(p+vec3(1,1,0)),f.x),f.y),
               mix(mix( hash(p+vec3(0,0,1)), 
                        hash(p+vec3(1,0,1)),f.x),
                   mix( hash(p+vec3(0,1,1)), 
                        hash(p+vec3(1,1,1)),f.x),f.y),f.z)-0.5;
}

float turb(vec3 uvw) {
    float c = 0.0;
    int i;
    float s1 = 2.0;
    float s2 = 1.0;
    float t = 0.0;
    for (i = 0; i < 7; ++i) {
        c += s2*noise(s1*uvw);
        //c += s2*noise1(s1*uv);
        t += s2;
        s1 *= 2.0;
        s2 *= 1.0;
    }
    return c/t;
}

float concrete(vec3 uvw) {
    float d = turb(1.0*uvw+vec3(23.45, 763.1, -123.2));
    float c = 0.1+0.26*smoothstep(-0.4, 0.4, d);

    d = turb(1.0*uvw+vec3(-13.12, 1245.0, 12.11));
    c += -0.1*smoothstep(0, 0.1, d);

    //d = turb(40.0*uv+vec2(12.0, 152.11));
    //c += d > 0.2 ? 0.1 : 0.0;

    return 0.5+1.0*c;
}
float theTime = 0.0;

mat4 translate(vec3 p) {
    return mat4(1.0,  0.0,  0.0,  -p.x,
              0.0,  1.0,  0.0,  -p.y,
              0.0,  0.0,  1.0,  -p.z,
              0.0, 0.0, 0.0, 1.0);
}

mat4 scale(vec3 s) {
    return mat4(s.x,  0.0,  0.0,  0.0,
              0.0,  s.y,  0.0,  0.0,
              0.0,  0.0,  s.z,  0.0,
              0.0, 0.0, 0.0, 1.0);
}

mat4 rotateZ(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat4(c,  -s,  0.0,  0.0,
              s,  c,  0.0,  0.0,
              0.0,  0.0,  1.0,  0.0,
              0.0, 0.0, 0.0, 1.0);
}

mat4 rotateY(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat4(c,  0.0, s,  0.0,
              0.0,  1.0, 0.0, 0.0,
              -s,  0.0, c,  0.0,
              0.0, 0.0, 0.0, 1.0);
}

mat4 rotateX(float theta) {
    float c = cos(theta);
    float s = sin(theta);
    return mat4(1.0, 0.0, 0.0, 0.0,
                0.0, c,  -s,  0.0, 
                0.0, s,  c,  0.0,
                0.0,  0.0,  0.0,  1.0);
}

// Assume n normalised
vec3 fold(vec3 n, vec3 x) {
    float nx = dot(n, x);
    return nx >= 0.0 ? x : x-2.0*nx*n;
}

vec4 reflect(vec3 n, vec3 p, vec4 x) {
    vec3 xyz = x.xyz-p;
    n = normalize(n);
    float nx = dot(n, xyz);
    vec4 nn;
    nn.xyz = p+(nx >= 0.0 ? xyz : xyz-2.0*nx*n);
    nn.w = 2.0*x.w+(nx >= 0 ? 1.0 : 0.0);
    return nn;
}

float g(vec3 x) {
    //return length(x)-0.9;
    return max(x.z,max(-x.z,max(x.y,max(-x.y,max(x.x, -x.x)))))-0.9;
}

#if 0
float h(vec3 x) {
    mat4 m = rotateZ(0.2101*theTime-3.128)*rotateY(0.1311*theTime+1.234);
    return g((vec4(x, 1.0)*m).xyz);
}
#endif

vec2 xy;

// {{1., 0., 0.}, {0., -1., 0.}, {0.809017, 0.5, -0.309017}}
//vec3 pc = vec3(1.95, -0.82, -0.94);
//vec3 pc = vec3(0.80, 0.5, -0.3);
vec3 pc = vec3(0.809017, 0.5, -0.309017);

vec3 dodecafold(vec3 x) {
    x.xy = -abs(x.xy);
    x = fold(pc, x);
    x.xy = -abs(x.xy);
    x = fold(pc, x);
    x.xy = -abs(x.xy);
    x = fold(pc, x);
    x.xy = -abs(x.xy);
    x = fold(pc, x);
    x.xy = -abs(x.xy);
    x = fold(pc, x);

    return x;
}

vec4 f(vec3 x0) {
    vec4 x = vec4(x0, 0.0);
    x = reflect(vec3(1.0, 0.0, 1.00), vec3(-4.3, 8.00, -19.01), x);
    x = reflect(vec3(0.0, 1.0, 1.00), vec3(4.3, -8.00, 1.01), x);
    x = reflect(vec3(1.0, 0.0, 0.00), vec3(-4.3, 0.00, 1.01), x);
    x = reflect(vec3(0.0, -2.0, 0.00), vec3(1.3, 1.00, 2.01), x);
    x = reflect(vec3(0.0, -1.0, 1.00), vec3(3.3, 1.01, -1.01), x);
    x = reflect(vec3(1.01, 0.0, 1.0), vec3(-1.01, 1.2, -3.01), x);
    x = reflect(vec3(0.0, 1.00, 1.0), vec3(0.0, -2.0, -7.0), x);
    x = reflect(vec3(1.0, 0.0, 1.0), vec3(0.01, -1.0, -1.0), x);
    x = reflect(vec3(1.0, 1.0, 0.0), vec3(-3.0, 1.0, 4.0), x);
    x = reflect(vec3(-1.0, 1.0, 0.0), vec3(0.0, -3.0, 2.0), x);
    x = reflect(vec3(-1.0, 0.0, 1.0), vec3(7.0, -5.0, 4.01), x);
    x = reflect(vec3(-1.0, 1.0, 0.0), vec3(1.0, 0.0, -2.0), x);
    x = reflect(vec3(1.0, 0.0, 0.0), vec3(-1.0, -2.0, 0.0), x);
    x = reflect(vec3(0.0, 1.0, 0.0), vec3(1.0, -3.0, -1.0), x);
    x = reflect(vec3(0.0, 1.0, 1.0), vec3(1.0, -4.0, -1.0), x);
    x = reflect(vec3(0.0, -1.0, 1.0), vec3(1.0, 2.0, 0.0), x);
    x = reflect(vec3(1.0, 1.0, 0.0), vec3(-4.0, 0.0, -1.0), x);

    return vec4(x.xyz, g(x.xyz));
}

float eps = 0.0001;
float lambda = 2.0;

vec3 ico[12];

//vec3 hash3(vec3 x) {
//    float u = 1000.0*sin(x.x*x.y+3.3*x.z-2.2*x.y+10.123*x.y+11.12*x.y*x.z);
//    float v = 1000.0*sin(x.z*x.y-2.1*x.z+3.0*x.z+7.211*x.y+32.12*x.y*x.x);
//    float w = 1000.0*cos(x.z*x.y+3.4*x.z-3.0*x.x+17.97*x.y+11.12*x.x*x.y);
//    return vec3(u-floor(u), v-floor(v), w-floor(w));
//}

float lighting(vec3 x, vec3 n) {
    float t = 0.0;
    for (int i = 0; i < 12; ++i) {
        //float p = f(x-0.05*n+0.10*ico[i]);
        //t += 0.8+10.0*p;
        float p = f(x+1.0*n+2.0*ico[i]).w;
        t += 0.5*p;
    }
    return t/12.0-0.5;
}

mat4 view() {
    return rotateY(0.2*theTime)*rotateX(0.00*theTime);
}

vec3 march(vec3 p, vec3 d) {
    float c;
    c = f(p).w;
    if (c < 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    for (int i = 0; i < 70; ++i) {
        float step = max(0.001, c);
        p = p+step*d;
        vec4 ff = f(p);
        vec3 x = ff.xyz;
        c = ff.w;
        if (c <= 0.0) {
            float ex, ey, ez;
            ex = f(p+vec3(eps, 0.0, 0.0)).w;
            ey = f(p+vec3(0.0, eps, 0.0)).w;
            ez = f(p+vec3(0.0, 0.0, eps)).w;
            vec3 n = vec3(ex-c, ey-c, ez-c)/eps;
            n = normalize(n);
            mat4 m = view();
            vec3 light = (vec4(1.0,1.0,-1.0,1.0)*m).xyz;
            float l0 = 0.25+0.75*max(dot(n, light)/sqrt(3.0), 0.0);
            float l1 = 0.5*lighting(p, n);
            float s = concrete(0.5*p);
            return 2.5*s*(0.1+0.7*l0+0.7*l1)*vec3(0.74, 0.72, 0.7);
        }
    }
    return vec3(0.1, 0.1, 0.1);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    theTime = 0.5*iTime+40.0;
    pc = normalize(pc);
    //theTime = 0.0;//iMouse.x;

    ico[0] = vec3(-0.26286500, 0.0000000, 0.42532500);
    ico[1] = vec3(0.26286500, 0.0000000, 0.42532500);
    ico[2] = vec3(-0.26286500, 0.0000000, -0.42532500);
    ico[3] = vec3(0.26286500, 0.0000000, -0.42532500);
    ico[4] = vec3(0.0000000, 0.42532500, 0.26286500);
    ico[5] = vec3(0.0000000, 0.42532500, -0.26286500);
    ico[6] = vec3(0.0000000, -0.42532500, 0.26286500);
    ico[7] = vec3(0.0000000, -0.42532500, -0.26286500);
    ico[8] = vec3(0.42532500, 0.26286500, 0.0000000);
    ico[9] = vec3(-0.42532500, 0.26286500, 0.0000000);
    ico[10] = vec3(0.42532500, -0.26286500, 0.0000000);
    ico[11] = vec3(-0.42532500, -0.26286500, 0.0000000);    
    
    vec2 uv = fragCoord.xy-0.5*iResolution.xy;
    uv = 2.0*uv/iResolution.y;
    xy = iMouse.xy/iResolution.xy;

    vec3 p = vec3(0.0, 0.0, -20.8);
    vec3 d = normalize(vec3(0.5*uv, 1.0));
    mat4 m = view();
    p = (vec4(p, 1.0)*m).xyz;
    d = (vec4(d, 1.0)*m).xyz;
    vec3 color = march(p, d);
    fragColor = vec4(color, 1.0);
}


