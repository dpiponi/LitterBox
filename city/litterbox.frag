vec3 target;

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

float cube(vec3 x) {
    //return length(x)-0.9;
    return max(x.z,max(-x.z,max(x.y,max(-x.y,max(x.x, -x.x)))))-1.25;
}

float sphere(vec3 x) {
    return length(x)-0.25;
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

#define N 25
vec3 path(float t) {
    vec3 pos[N];
    pos[0] = vec3(-3.75, -3.2, -18.0);
    pos[1] = vec3(-4.0, -2.5, -14.0);
    pos[2] = vec3(-5.0, 0.0, -10.0);
    pos[3] = vec3(-3.0, 0.0, -9.0);
    pos[4] = vec3(-4.0, 0.0, -10.0);
    pos[5] = vec3(-10.0, 1.0, -8.0);
    pos[6] = vec3(-10.0, 0.0, -5.0);
    pos[7] = vec3(-10.0, 3.1, -2.0);
    pos[8] = vec3(-9.0, 5.0, 0.0);
    pos[9] = vec3(-7.0, 5.0, 0.0);
    pos[10] = vec3(-6.0, 6.0, -2.0);
    pos[11] = vec3(-3.5, 8.1, -8.0);
    pos[12] = vec3(-5.6, 5.0, -8.0);
    pos[13] = vec3(-8.4, 4.5, -5.0);
    pos[14] = vec3(-3.3, 7.2, -8.2);
    pos[15] = vec3(-4.7, 6.4, -7.0);
    pos[16] = vec3(-2.5, 4.2, -12.0);
    pos[17] = vec3(-2.5, 1.0, -22.0);
    pos[18] = vec3(-4.5, -2.0, -16.0);
    pos[19] = vec3(-4.5, -2.0, -17.0);
    pos[20] = vec3(-4.3, -3.5, -17.0);
    pos[21] = vec3(-4.0, -5.75, -16.0);
    pos[22] = vec3(-4.5, -7.0, -15.0);
    pos[23] = vec3(-5.5, -8.0, -14.0);
    pos[24] = vec3(-6.5, -6.6, -13.0);

    int it = int(floor(t));
    while (it >= N) {
        it -= N;
    }
    float ft = fract(t);
    vec3 qos0 = pos[it];
    ++it;
    if (it >= N) {
        it = 0;
    }
    vec3 qos1 = pos[it];
    ++it;
    if (it >= N) {
        it = 0;
    }
    vec3 qos2 = pos[it];
    ++it;
    if (it >= N) {
        it = 0;
    }
    vec3 qos3 = pos[it];
    vec3 pos0 = 2*qos1;
    vec3 pos1 = -0.333333*qos0+2.0*qos1+0.333333*qos2;
    vec3 pos2 = 0.333333*qos1+2.0*qos2-0.333333*qos3;
    vec3 pos3 = 2*qos2;
    pos0 = mix(pos0, pos1, ft);
    pos1 = mix(pos1, pos2, ft);
    pos2 = mix(pos2, pos3, ft);
    pos0 = mix(pos0, pos1, ft);
    pos1 = mix(pos1, pos2, ft);
    pos0 = mix(pos0, pos1, ft);
    return pos0;
}

vec4 scene(vec3 x0) {
    vec4 x = vec4(x0, 0.0);
    x = reflect(vec3(0.0, 1.0, 1.00), vec3(5.0, 5.00, -11.00), x);
    x = reflect(vec3(1.0, 0.0, -1.00), vec3(-3.0, 5.00, 11.00), x);
    x = reflect(vec3(0.0, 1.0, -1.00), vec3(-2.0, 6.00, 8.00), x);
    x = reflect(vec3(1.0, -1.0, 0.00), vec3(-4.0, 21.00, -8.00), x);
    x = reflect(vec3(1.0, 0.0, 0.00), vec3(-4.0, 12.00, -10.00), x);
    x = reflect(vec3(0.0, 0.0, 1.00), vec3(4.0, 10.00, -8.00), x);
    x = reflect(vec3(1.0, 0.0, 1.00), vec3(-1.0, 8.00, -2.00), x);
    x = reflect(vec3(0.0, 1.0, 1.00), vec3(8.0, -10.00, 4.00), x);
    x = reflect(vec3(1.0, -1.0, 0.00), vec3(-6.0, 4.00, 1.00), x);
    x = reflect(vec3(0.0, -1.0, 0.00), vec3(2.0, 1.00, 3.00), x);
    x = reflect(vec3(0.0, -1.0, 1.00), vec3(4.0, 0.00, 0.00), x);
    x = reflect(vec3(1.00, 0.0, 1.0), vec3(-2.00, 5.0, -6.00), x);
    x = reflect(vec3(0.0, 1.00, 1.0), vec3(1.0, -4.0, -4.0), x);
    x = reflect(vec3(1.0, 1.0, 0.0), vec3(-5.00, -1.0, -1.0), x);
    x = reflect(vec3(1.0, -1.0, 0.0), vec3(-7.0, -4.0, 8.0), x);
    x = reflect(vec3(-1.0, 1.0, 0.0), vec3(0.0, -3.0, 2.0), x);
    x = reflect(vec3(-1.0, 0.0, 1.0), vec3(7.0, -5.0, 4.00), x);
    x = reflect(vec3(-1.0, 1.0, 0.0), vec3(1.0, -0.0, -2.0), x);
    x = reflect(vec3(1.0, 1.0, 0.0), vec3(6.0, -8.0, 0.0), x);
    x = reflect(vec3(0.0, 1.0, 0.0), vec3(-1.0, -4.0, -1.0), x);
    x = reflect(vec3(0.0, 1.0, 1.0), vec3(1.0, -5.0, -3.0), x);
    x = reflect(vec3(0.0, -1.0, 1.0), vec3(2.0, 3.0, 0.0), x);
    x = reflect(vec3(1.0, 0.0, 1.0), vec3(1.0, -5.0, -3.0), x);
    //x = rotateZ(0.25*theTime)*x;
    //x = rotateY(0.5*theTime)*x;

//    float d = cube(x.xyz);
//    for (int i = 0; i < 5; ++i) {
//        d = min(d, sphere(x0-path(theTime+0.5+0.5*i)));
//    }
//    return vec4(x.xyz, d);
    return vec4(x.xyz, min(cube(x.xyz), sphere(x0-target)));
    //return vec4(x.xyz, sphere(x0-target));
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
        //float p = scene(x-0.05*n+0.10*ico[i]);
        //t += 0.8+10.0*p;
        float p = scene(x+1.0*n+2.0*ico[i]).w;
        t += 0.5*p;
    }
    return t/12.0-0.5;
}

mat4 view() {
    return rotateY(0.25*theTime)*rotateX(0.00*theTime);
}

vec3 march(vec3 p, vec3 d) {
    float c;
    c = scene(p).w;
    if (c < 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }
    for (int i = 0; i < 100; ++i) {
        float step = max(0.004, c);
        p = p+step*d;
        vec4 ff = scene(p);
        vec3 x = ff.xyz;
        c = ff.w;
        if (c <= 0.0) {
            float ex, ey, ez;
            ex = scene(p+vec3(eps, 0.0, 0.0)).w;
            ey = scene(p+vec3(0.0, eps, 0.0)).w;
            ez = scene(p+vec3(0.0, 0.0, eps)).w;
            vec3 n = vec3(ex-c, ey-c, ez-c)/eps;
            n = normalize(n);
            mat4 m = view();
            vec3 light = (vec4(1.0,1.0,-1.0,1.0)*m).xyz;
            float l0 = 0.25+0.75*max(dot(n, light)/sqrt(3.0), 0.0);
            float l1 = 0.75*lighting(p, n);
            float s = concrete(0.5*p);
            //return 2.5*s*(0.1+0.7*l0+0.7*l1)*vec3(0.74, 0.72, 0.7);
            return 1.1*s*(0.3+0.5*l0+0.7*l1)*vec3(1.61, 1.25, 0.96);
        }
    }
    return vec3(0.1, 0.1, 0.1);
}

mat3 complete(vec3 y, vec3 z) {
    vec3 x = normalize(cross(y, z));
    y = normalize(cross(z, x));
    return mat3(x, y, z);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    theTime = 0.5*iTime;
    pc = normalize(pc);

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

//    theTime = N*xy.x;
//    theTime = 10.95;
//    theTime = 17+mod(theTime, 4.0);

    vec3 p = path(theTime);//ec3(-4.0, 0.0, -41.0);
    vec3 z = normalize(path(theTime+0.2)-p);
    mat3 m = complete(vec3(0.0, 1.0, 0.0), z);
    vec3 d = normalize(vec3(0.5*uv, 1.0));
    target = path(theTime+0.5);
    d = m*d;
    //mat4 m = view();
    //p = (m*vec4(p, 1.0)).xyz;
    //d = (m*vec4(d, 1.0)).xyz;
    vec3 color = march(p, d);
    fragColor = vec4(color, 1.0);
}


