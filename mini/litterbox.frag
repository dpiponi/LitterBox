float rand(inout float seed) {
    seed = seed+0.63;
    float s = 1000.0*(seed+cos(17.0*seed-22.1*seed*seed));
    seed = 11.37*seed+1.2;
    seed = seed-floor(seed);
    return s-floor(s);
}

float pi = 3.1415926;

vec3 vectorOutOfPlane(inout float seed, vec3 n) {
    float z = -1.0+2.0*rand(seed);
    float rxy = sqrt(1-z*z);
    float phi = 2.0*pi*rand(seed);
    float x = rxy*cos(phi);
    float y = rxy*sin(phi);
    vec3 d = vec3(x, y, z);
    return dot(n, d) < 0.0 ? -d : d;
}

void doRotateX(inout mat4 m, inout mat4 im, float theta) {
    float c = cos(theta);
    float s = sin(theta);
    m = m*mat4(1.0, 0.0, 0.0, 0.0,
                0.0, c, s, 0.0,
                0.0, -s, c, 0.0,
                0.0, 0.0, 0.0, 1.0);
    im = mat4(1.0, 0.0, 0.0, 0.0,
                0.0, c, -s, 0.0,
                0.0, s, c, 0.0,
                0.0, 0.0, 0.0, 1.0)*im;
}

void doRotateY(inout mat4 m, inout mat4 im, float theta) {
    float c = cos(theta);
    float s = sin(theta);
    m = m*mat4(c, 0.0, -s, 0.0,
                0.0, 1.0, 0.0, 0.0,
                s, 0.0, c, 0.0,
                0.0, 0.0, 0.0, 1.0);
    im = mat4(c, 0.0, s, 0.0,
                0.0, 1.0, 0.0, 0.0,
                -s, 0.0, c, 0.0,
                0.0, 0.0, 0.0, 1.0)*im;
}

void doTranslate(inout mat4 m, inout mat4 im, vec3 p) {
    m = m*mat4(1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                p.x, p.y, p.z, 1.0);
    im = mat4(1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                -p.x, -p.y, -p.z, 1.0)*im;
}

void doScale(inout mat4 m, inout mat4 im, float sx, float sy, float sz) {
    m = m*mat4(sx, 0.0, 0.0, 0.0,
                0.0, sy, 0.0, 0.0,
                0.0, 0.0, sz, 0.0,
                0.0, 0.0, 0.0, 1.0);
    im = mat4(1.0/sx, 0.0, 0.0, 0.0,
                0.0, 1.0/sy, 0.0, 0.0,
                0.0, 0.0, 1.0/sz, 0.0,
                0.0, 0.0, 0.0, 1.0)*im;
}

bool inner(inout vec3 normal, vec4 n, inout float near, inout float far, vec3 p, vec3 d) {
        float np = dot(n, vec4(p, 1.0));
        float nd = dot(n, vec4(d, 0.0));
        if (nd == 0.0) {
            // Parallel
            if (np > 0.0) {
                // All outside
                return false;
            }
        } else {
            // Find t at intersection
            float t = -np/nd; // -p.z/d.z
            if (nd < 0.0) {
                // Entering
                if (t > near) {
                    normal = n.xyz;
                    near = t;
                }
            } else {
                // Exiting
                far = min(far, t);
            }
        }
        return true;
}

bool intersectCube(out float near, out vec3 normal, vec3 p, vec3 d) {
    near = 0.0;
    float far = 1000.0;
    if (!inner(normal, vec4(0.0, 0.0, -1.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(0.0, 0.0, 1.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(-1.0, 0.0, 0.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(1.0, 0.0, 0.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(0.0, -1.0, 0.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(0.0, 1.0, 0.0, -1.0), near, far, p, d)) { return false; }
    if (near > 0.0 && near < far) {
        return true;
    } else {
        return false;
    }
}

bool intersectCylinder(out float near, out vec3 normal, vec3 p, vec3 d) {
    near = 0.0;
    float far = 1000.0;
    if (!inner(normal, vec4(0.0, 0.0, -1.0, -1.0), near, far, p, d)) { return false; }
    if (!inner(normal, vec4(0.0, 0.0, 1.0, -1.0), near, far, p, d)) { return false; }
    float dd = dot(d.xy, d.xy);
    float dp = dot(d.xy, p.xy);
    float pp = dot(p.xy, p.xy);
    float disc = dp*dp-dd*(pp-1.0);
    if (disc < 0.0) {
        return false;
    }
    float sqdisc = sqrt(disc);
    float t0 = (-dp-sqdisc)/dd;
    if (t0 > near) {
        normal = vec3((p+near*d).xy, 0.0);
        near = t0;
    }
    float t1 = (-dp+sqdisc)/dd;
    far = min(far, t1);
    return near < far;
}

bool intersectSphere(out float near, out vec3 normal, vec3 p, vec3 d) {
    near = 0.0;
    float far = 1000.0;
    float dd = dot(d, d);
    float dp = dot(d, p);
    float pp = dot(p, p);
    float disc = dp*dp-dd*(pp-1.0);
    if (disc < 0.0) {
        return false;
    }
    float sqdisc = sqrt(disc);
    float t0 = (-dp-sqdisc)/dd;
    float t1 = (-dp+sqdisc)/dd;
    far = min(far, t1);
    if (t0 > near) {
        near = t0;
        normal = p+near*d;
        return near < far;
    }
    return false;
}

bool intersectScene(out float near, out vec3 normal, vec3 p, vec3 d, bool debug) {
    bool did = false;
    near = 1e8;

    mat4 m = mat4(1.0);
    mat4 im = mat4(1.0);
    doTranslate(m, im, vec3(0.6, 0.4, 0.0));
    doRotateY(m, im, 0.1*iTime);
    doRotateX(m, im, 0.1*iTime);
    doScale(m, im, 0.5, 1.5, 0.05);
    vec3 new_p = (im*vec4(p, 1.0)).xyz;
    vec3 new_d = (im*vec4(d, 0.0)).xyz;
    float new_near;
    vec3 new_normal;
    if (intersectCylinder(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

    m = mat4(1.0);
    im = mat4(1.0);
    doTranslate(m, im, vec3(0.6, -0.4, 0.0));
    doRotateY(m, im, 0.3*iTime);
    doRotateX(m, im, 0.3*iTime);
    doScale(m, im, 0.5, 0.5, 0.5);
    new_p = (im*vec4(p, 1.0)).xyz;
    new_d = (im*vec4(d, 0.0)).xyz;
    if (intersectSphere(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

    m = mat4(1.0);
    im = mat4(1.0);
    doTranslate(m, im, vec3(-0.6, 0.4, 0.0));
    doRotateX(m, im, 0.2*iTime);
    doRotateY(m, im, -0.3*iTime);
    doScale(m, im, 0.25, 0.25, 1.5);
    new_p = (im*vec4(p, 1.0)).xyz;
    new_d = (im*vec4(d, 0.0)).xyz;
    if (intersectCylinder(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

    m = mat4(1.0);
    im = mat4(1.0);
    doTranslate(m, im, vec3(-0.6, -0.4, 0.0));
    doRotateY(m, im, 0.5*iTime);
    doRotateX(m, im, 0.6*iTime);
    doScale(m, im, 0.5, 0.5, 0.5);
    new_p = (im*vec4(p, 1.0)).xyz;
    new_d = (im*vec4(d, 0.0)).xyz;
    if (intersectCube(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

    return did;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy-0.5*iResolution;
    uv = 2.0*uv/iResolution.y;

    float seed = uv.x+uv.y;
    seed = rand(seed)+uv.y-uv.x;
    seed = rand(seed)+iTime;
    seed = rand(seed);
    float focus = 0.0;
    float lens = -3.0;
    float aperture = 0.1;

    int count = 0;
    vec3 color = vec3(0.0, 0.0, 0.0);
    for (int k = 0; k < 256; ++k) {

        vec3 p = vec3(0.0, 0.0, -4.0);//+1.5*sin(0.2*iTime));
        vec3 d = vec3(0.5*uv, 1.0);
        float lambda_focus = (focus-p.z)/d.z;
        float lambda_lens = (lens-p.z)/d.z;

        vec3 new_p = p+lambda_lens*d+vec3(aperture*rand(seed)-0.5*aperture, aperture*rand(seed)-0.5*aperture, 0.0);;
        vec3 new_d = p+lambda_focus*d-new_p;

        p = new_p;
        d = new_d;

        vec3 normal;
        float near;
        if (intersectScene(near, normal, p, d, true)) {
            p = p+near*d;
            normal = normalize(normal);
            for (int i = 0; i < 1; ++i) {
                vec3 secondary = vectorOutOfPlane(seed, normal);
                //secondary = normal;
                vec3 new_p = p+0.0001*secondary;
                vec3 ignore;
                float ignore2;
                bool did = intersectScene(ignore2, ignore, new_p, secondary, true);
                if (!did) {
                    color += vec3(0.8, 0.8, 0.8);
                }
            }
            //fragColor = vec4(normal, 1.0);
        } else {
            color += vec3(0.1, 0.1, 0.1);
        }
        ++count;
    }
    float gain = 1.0/float(count);
    fragColor = vec4(gain*color, 1.0);
}
