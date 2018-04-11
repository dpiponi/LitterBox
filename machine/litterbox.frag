float rate = 0.1;

const vec3 zero3 = vec3(0.0, 0.0, 0.0);

struct Interval {
    float near;
    float far;
    vec3 normal;
    int material;
    vec3 t;
};

Interval nothing = Interval(1e8, 0.0, zero3, -1, zero3);
Interval everything = Interval(0.0, 1e8, zero3, -1, zero3);

struct Ray {
    vec3 p;
    vec3 d;
};

Ray multiplyMatRay(mat4 m, Ray ray) {
    return Ray(
        (vec4(ray.p, 1.0)*m).xyz,
        (vec4(ray.d, 0.0)*m).xyz);
}

void multiplyMatInterval(mat4 m, inout Interval a) {
    a.normal = (m*vec4(a.normal, 0.0)).xyz;
}

float rand(inout float seed) {
#if 1
    seed = seed+0.63;
    float s = 1000.0*(seed+cos(17.0*seed-22.1*seed*seed));
    seed = 11.37*seed+1.2;
    seed = seed-floor(seed);
    return s-floor(s);
#endif
#if 0
    float t = 0.12+1229.17*seed-1236.23*seed*seed+234.11*seed*seed*seed;
    seed = t-floor(t);
    return seed;
#endif
}

float pi = 3.1415926;

vec3 vectorOutOfPlane(inout float seed, vec3 n) {
    float z = -1.0+2.0*rand(seed);
    float rxy = sqrt(1.0-z*z);
    float phi = 2.0*pi*rand(seed);
    float x = rxy*cos(phi);
    float y = rxy*sin(phi);
    vec3 d = vec3(x, y, z);
    return dot(n, d) < 0.0 ? -d : d;
}

void doRotateX(inout mat4 im, float theta) {
    float c = cos(theta);
    float s = sin(theta);
    vec4 t = c*im[1]+s*im[2];
    im[2] = -s*im[1]+c*im[2];
    im[1] = t;
    return;
}

void doRotateY(inout mat4 im, float theta) {
    float c = cos(theta);
    float s = sin(theta);
    vec4 t = c*im[2]+s*im[0];
    im[0] = -s*im[2]+c*im[0];
    im[2] = t;
    return;
}

void doRotateZ(inout mat4 im, float theta) {
    float c = cos(theta);
    float s = sin(theta);
    vec4 t = c*im[0]+s*im[1];
    im[1] = -s*im[0]+c*im[1];
    im[0] = t;
    return;
}

void doTranslate(inout mat4 im, vec3 p) {
    im = im*mat4(1.0,  0.0,  0.0,  -p.x,
              0.0,  1.0,  0.0,  -p.y,
              0.0,  0.0,  1.0,  -p.z,
              0.0, 0.0, 0.0, 1.0);
}

void doScale(inout mat4 im, float sx, float sy, float sz) {
    im[0] /= sx;
    im[1] /= sy;
    im[2] /= sz;
}

bool empty(Interval a) {
    return a.near > a.far;
}

Interval intersect(Interval a, Interval b) {
#if 0
    if (empty(a)) {
        return nothing;
    }
    if (empty(b)) {
        return nothing;
    }
#endif
    Interval c;
    if (a.near > b.near) {
        c.near = a.near;
        c.normal = a.normal;
        c.material = a.material;
        c.t = a.t;
    } else {
        c.near = b.near;
        c.normal = b.normal;
        c.material = b.material;
        c.t = b.t;
    }
    if (a.far < b.far) {
        c.far = a.far;
    } else {
        c.far = b.far;
    }
    return c;
}

Interval union_(Interval a, Interval b) {
    if (empty(a)) {
        return b;
    }
    if (empty(b)) {
        return a;
    }
    Interval c;
    if (a.near < b.near) {
        c.near = a.near;
        c.normal = a.normal;
        c.material = a.material;
        c.t = a.t;
    } else {
        c.near = b.near;
        c.normal = b.normal;
        c.material = b.material;
        c.t = b.t;
    }
    if (a.far > b.far) {
        c.far = a.far;
    } else {
        c.far = b.far;
    }
    return c;
}

Interval rayPlane(vec4 n, Ray ray, int material) {
    float np = dot(n, vec4(ray.p, 1.0));
    float nd = dot(n, vec4(ray.d, 0.0));
    if (nd == 0.0) {
        // Parallel
        if (np > 0.0) {
            return nothing;
        } else {
            return everything;
        }
    } else {
        float t = -np/nd;
        if (nd < 0.0) {
            return Interval(t, 1e8, n.xyz, material, ray.p+t*ray.d);
        } else {
            return Interval(0.0, t, zero3, -1, zero3);
        }
    }
}

Interval cylinder(Ray ray, int material) {
    Interval a = rayPlane(vec4(0.0, 0.0, -1.0, -1.0), ray, material);
    if (empty(a)) { return a; }
    a = intersect(a, rayPlane(vec4(0.0, 0.0, 1.0, -1.0), ray, material));
    if (empty(a)) { return a; }

    float dd = dot(ray.d.xy, ray.d.xy);
    float dp = dot(ray.d.xy, ray.p.xy);
    float pp = dot(ray.p.xy, ray.p.xy);
    float disc = dp*dp-dd*(pp-1.0);
    if (disc < 0.0) {
        return nothing; // XXX Extreme case could happen
    }
    float sqdisc = sqrt(disc);
    float idd = 1.0/dd;
    float t0 = max(0.0, (-dp-sqdisc)*idd);
    float t1 = (-dp+sqdisc)*idd;
    vec3 q = vec3(ray.p.xy+t0*ray.d.xy, 0.0);
    a = intersect(a, Interval(t0, t1, q, material, ray.p+t0*ray.d));
    return a;
}

Interval sphere(Ray ray, int material) {
    float dd = dot(ray.d, ray.d);
    float dp = dot(ray.d, ray.p);
    float pp = dot(ray.p, ray.p);
    float disc = dp*dp-dd*(pp-1.0);
    if (disc < 0.0) {
        return nothing;
    }
    float sqdisc = sqrt(disc);
    float idd = 1.0/dd;
    float t0 = max(0.0, (-dp-sqdisc)*idd);
    float t1 = (-dp+sqdisc)*idd;
    vec3 q = ray.p+t0*ray.d;
    return Interval(t0, t1, q, material, q);
}

Interval cube(Ray ray, int material) {
    Interval a;
    a = rayPlane(vec4(0.0, 0.0, 1.0, -1.0), ray, material);
    if (empty(a)) { return a; }
    a = intersect(a, rayPlane(vec4(0.0, 0.0, -1.0, -1.0), ray, material));
    if (empty(a)) { return a; }
    a = intersect(a, rayPlane(vec4(1.0, 0.0, 0.0, -1.0), ray, material));
    if (empty(a)) { return a; }
    a = intersect(a, rayPlane(vec4(-1.0, 0.0, 0.0, -1.0), ray, material));
    if (empty(a)) { return a; }
    a = intersect(a, rayPlane(vec4(0.0, 1.0, 0.0, -1.0), ray, material));
    if (empty(a)) { return a; }
    return intersect(a, rayPlane(vec4(0.0, -1.0, 0.0, -1.0), ray, material));
}

Interval plate(mat4 m, Ray ray) {
    doRotateX(m, 0.5*pi);
    doScale(m, 0.1, 0.1, 0.02);
    Interval a = cylinder(multiplyMatRay(m, ray), 3);
    multiplyMatInterval(m, a);
    return a;
}

Interval rod(mat4 m, Ray ray) {
    doTranslate(m, vec3(0.0, 0.4, 0.0));
    doRotateX(m, 0.5*pi);
    doScale(m, 0.01, 0.01, 0.4);
    Interval a = cylinder(multiplyMatRay(m, ray), 5);
    multiplyMatInterval(m, a);
    return a;
}

Interval cam(mat4 m, Ray ray, float pos, float phase) {
    doRotateX(m, 1.0*iTime+phase);
    doTranslate(m, vec3(pos, 0.1, 0.0));
    doRotateY(m, 0.5*pi);
    doScale(m, 0.15, 0.2, 0.02);
    Interval a = cylinder(multiplyMatRay(m, ray), 3);
    multiplyMatInterval(m, a);
    return a;
}

Interval axle(mat4 m, Ray ray) {
    doRotateY(m, 0.5*pi);
    doScale(m, 0.05, 0.05, 1.0);
    Interval a = cylinder(multiplyMatRay(m, ray), 3);
    multiplyMatInterval(m, a);
    return a;
}

bool axlebox(mat4 m, Ray ray) {
    doScale(m, 1.1, 0.34, 0.3);
    Interval a = cube(multiplyMatRay(m, ray), -1);
    return empty(a);
}

Interval axleAssembly(mat4 m, Ray ray) {
    doTranslate(m, vec3(0.0, -0.6, 0.0));
    if (axlebox(m, ray)) {
        return nothing;
    }
    Interval a = cam(m, ray, 0.6, 0.0);
    Interval b = union_(a, cam(m, ray, -0.6, 0.5*pi));
    Interval c = union_(b, axle(m, ray));
    return c;
}


Interval blob(mat4 m, Ray ray, vec3 t, vec3 s) {
    doTranslate(m, t);
    doScale(m, s.x, s.y, s.z);
    Interval a = cylinder(multiplyMatRay(m, ray), 6);
    multiplyMatInterval(m, a);
    return a;
}

bool cloudbox(mat4 m, Ray ray) {
    doTranslate(m, vec3(0.0, -0.1, 0.0));
    doScale(m, 0.9, 0.55, 0.025);
    Interval a = cube(multiplyMatRay(m, ray), -1);
    return empty(a);
}

Interval front(mat4 m, Ray ray) {
    //return nothing;
    doTranslate(m, vec3(0.0, -0.2, -0.3));

    if (cloudbox(m, ray)) {
        return nothing;
    }

    Interval g = blob(m, ray, vec3(0.0, -0.1, 0.0), vec3(0.4, 0.35, 0.025));
    Interval a = union_(g, blob(m, ray, vec3(-0.2, 0.1, 0.0), vec3(0.3, 0.205, 0.025)));
    Interval b = union_(a, blob(m, ray, vec3(0.2, 0.1, 0.0), vec3(0.3, 0.2, 0.025)));
    Interval c = union_(b, blob(m, ray, vec3(-0.5, -0.1, 0.0), vec3(0.35, 0.2, 0.025)));
    Interval d = union_(c, blob(m, ray, vec3(0.5, -0.1, 0.0), vec3(0.33, 0.21, 0.025)));
    Interval e = union_(d, blob(m, ray, vec3(-0.26, -0.35, 0.0), vec3(0.35, 0.23, 0.025)));
    Interval f = union_(e, blob(m, ray, vec3(0.3, -0.4, 0.0), vec3(0.26, 0.18, 0.025)));

    return f;
}

Interval back(mat4 m, Ray ray) {
    doTranslate(m, vec3(0.0, 0.3, 0.3));
    doScale(m, 1.2, 0.8, 0.025);
    Interval a = cube(multiplyMatRay(m, ray), 7);
    multiplyMatInterval(m, a);
    return a;
}

Interval envelope(mat4 m, Ray ray) {
    doScale(m, 1.0, 0.25, 0.25);
    Interval a = sphere(multiplyMatRay(m, ray), 4);
    multiplyMatInterval(m, a);
    return a;
}

Interval cabin(mat4 m, Ray ray) {
    doTranslate(m, vec3(-0.4, -0.23, 0.0));
    doScale(m, 0.1, 0.05, 0.1);
    Interval a = cube(multiplyMatRay(m, ray), 1);
    multiplyMatInterval(m, a);
    return a;
}

Interval fin(mat4 m, Ray ray, float angle, float pos) {
    doTranslate(m, vec3(0.9, pos, 0.0));
    doRotateZ(m, angle);
    doScale(m, 0.15, 0.1, 0.025);
    Interval a = cylinder(multiplyMatRay(m, ray), 4);
    multiplyMatInterval(m, a);
    return a;
}

Interval fin1(mat4 m, Ray ray) {
    return fin(m, ray, 0.3, 0.13);
}

Interval fin2(mat4 m, Ray ray) {
    return fin(m, ray, -0.3, -0.13);
}

bool shipbox(mat4 m, Ray ray) {
    doScale(m, 1.1, 0.3, 0.25);
    Interval a = cube(multiplyMatRay(m, ray), -1);
    return empty(a);
}

float pistonHeight(float phase, float t) {
    return 0.10+0.21*0.5*(1.0+sin(1.0*iTime+phase)-0.35*sin(2.0*(iTime+phase)+0.5*pi));
}

Interval ship(mat4 m, Ray ray) {
    //float bob = 0.1*sin(iTime+0.5*pi);
    float h0 = pistonHeight(0.5*pi, iTime);
    float h1 = pistonHeight(1.0*pi, iTime);
    float bob = 0.5*(h0+h1)-0.2;
    doTranslate(m, vec3(0.0, 0.5+bob, 0.0));
    //float angle = -0.15*sin(iTime-0.75*pi);
    float angle = 1.1*(h0-h1);
    doRotateZ(m, angle);

    if (shipbox(m, ray)) {
        return nothing;
    }

    Interval a = envelope(m, ray);
    Interval b = union_(a, cabin(m, ray));
    Interval c = union_(b, fin1(m, ray));
    Interval d = union_(c, fin2(m, ray));

    return d;
}

Interval piston(mat4 m, Ray ray, float pos, float phase) {
    doTranslate(m, vec3(0.0, -0.6, 0.0));
    doTranslate(m, vec3(pos, pistonHeight(phase, iTime), 0.0));
    Interval a = plate(m, ray);
    Interval b = union_(a, rod(m, ray));
    multiplyMatInterval(m, b);
    return b;
}

Interval scene(Ray ray) {
    mat4 m = mat4(1.0);
    doRotateY(m, 0.5*pi*(iMouse.x/iResolution.x));
    Interval a = axleAssembly(m, ray);
    Interval b = union_(a, piston(m, ray, 0.6, 0.5*pi));
    Interval c = union_(b, piston(m, ray, -0.6, 1.0*pi));

    Interval d = union_(c, front(m, ray));
    Interval e = union_(d, back(m, ray));
    Interval f = union_(e, ship(m, ray));

    return f;
}

vec3 material_color(int material, vec3 p, vec3 t) {
    if (material == 1) {
        // white
        return vec3(1.0, 1.0, 1.0);
    } else if (material == 2) {
        // sky
        return vec3(0.52, 0.8, 0.92);
    } else if (material == 3) {
        // wood
        vec3 a = vec3(0.8, 0.6, 0.5);
        vec3 b = vec3(0.9, 0.9, 0.8);
        float u = 0.5+0.5*sin(pi*20.0*t.y);
        return mix(a, b, u);
    } else if (material == 4) {
        // ship
        if (t.y > -0.05 && t.y < 0.05) {
            return vec3(0.9, 0.6, 0.2);
        }
        return vec3(0.85, 0.85, 0.85);
    } else if (material == 5) {
        // brass
        return vec3(0.8, 0.6, 0.5);
    } else if (material == 6) {
        // cloud
        vec3 a = vec3(0.52, 0.8, 0.92);
        vec3 b = vec3(1.0, 1.0, 1.0);
        float u = 0.9+1.8*p.y;
        return mix(a, b, u);
    } else if (material == 7) {
        // sky
        vec3 a = vec3(0.55, 0.82, 0.92);
        vec3 b = vec3(0.84, 0.6, 0.59);
        float u = 0.5+0.5*1.0*t.y;
        u = clamp(u, 0.0, 1.0);
        return mix(b, a, u);
    }
    return zero3;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy-0.5*iResolution.xy;
    uv = 2.0*uv/iResolution.y;

    float seed = uv.x+uv.y;
    seed = rand(seed)+uv.y-uv.x;
    seed = rand(seed)+iTime;
    seed = rand(seed);
    float focus = 0.0;
    float lens = -3.0;
    float aperture = 0.0375;

    int count = 0;
    vec3 color = zero3;
    for (int k = 0; k < 64; ++k) {
        vec3 p = vec3(0.0, 0.0, -2.0);
        vec3 d = vec3(0.5*uv, 1.0);
        float iz = 1.0/d.z;
        float lambda_focus = (focus-p.z)*iz;
        float lambda_lens = (lens-p.z)*iz;

        vec3 new_p = p+vec3(aperture*rand(seed)-0.5*aperture, aperture*rand(seed)-0.5*aperture, lambda_lens);
        vec3 new_d = p+lambda_focus*d-new_p;

        p = new_p;
        d = new_d;

        vec3 normal;
        Interval a = scene(Ray(p, d));
        if (!empty(a) && a.near > 0.0) {
            p = p+a.near*d;
            normal = normalize(a.normal);
            for (int i = 0; i < 1; ++i) {
                vec3 secondary;
                if (a.material == 5) {
                    secondary = reflect(d, normal);
                } else {
                    secondary = vectorOutOfPlane(seed, normal);
                }
                if (rand(seed) < 0.05) {
                    secondary = vec3(1.0, 0.5, -0.5);
                }
                Interval b = scene(Ray(p+0.00001*normal, secondary));
                if (empty(b) || b.near < 0.0) {
                    if (a.material == 5) {
                    } else {
                        color += material_color(a.material, p, a.t);
                    };
                } else {
                    if (a.material == 5) {
                        color += material_color(5, new_p, a.t)*material_color(b.material, new_p+b.near*secondary, b.t);
                    } else {
                    }
                }
            }
        } else {
            color += vec3(0.3, 0.3, 0.4);
        }
        ++count;
    }
    float gain = 1.0/float(count);
    fragColor = vec4(gain*color, 1.0);
}
