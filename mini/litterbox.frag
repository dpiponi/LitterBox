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

mat4 rotateX(float theta) {
    float s = sin(theta);
    float c = cos(theta);
    return mat4(1.0, 0.0, 0.0, 0.0,
                0.0, c, s, 0.0,
                0.0, -s, c, 0.0,
                0.0, 0.0, 0.0, 1.0);
}

mat4 invRotateX(float theta) {
    return rotateX(-theta);
}

mat4 rotateY(float theta) {
    float s = sin(theta);
    float c = cos(theta);
    return mat4(c, 0.0, -s, 0.0,
                0.0, 1.0, 0.0, 0.0,
                s, 0.0, c, 0.0,
                0.0, 0.0, 0.0, 1.0);
}

mat4 invRotateY(float theta) {
    return rotateY(-theta);
}

mat4 translate(vec3 p) {
    return mat4(1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                p.x, p.y, p.z, 1.0);
}


mat4 invTranslate(vec3 p) {
    return translate(-p);
}

mat4 scale(float s) {
    return mat4(s, 0.0, 0.0, 0.0,
                0.0, s, 0.0, 0.0,
                0.0, 0.0, s, 0.0,
                0.0, 0.0, 0.0, 1.0);
}

mat4 invScale(float s) {
    return scale(1.0/s);
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

    mat4 m = translate(vec3(0.6, 0.4, 0.0));
    mat4 im = invTranslate(vec3(0.6, 0.4, 0.0));
    m = m*rotateY(0.1*iTime);
    im = invRotateY(0.1*iTime)*im;
    m = m*rotateX(0.4*iTime);
    im = invRotateX(0.4*iTime)*im;
    m = m*scale(0.5);
    im = invScale(0.5)*im;
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

    if (debug) {
    m = translate(vec3(0.6, -0.4, 0.0));
    im = invTranslate(vec3(0.6, -0.4, 0.0));
    m = m*rotateY(0.3*iTime);
    im = invRotateY(0.3*iTime)*im;
    m = m*rotateX(0.3*iTime);
    im = invRotateX(0.3*iTime)*im;
    m = m*scale(0.5);
    im = invScale(0.5)*im;
    new_p = (im*vec4(p, 1.0)).xyz;
    new_d = (im*vec4(d, 0.0)).xyz;
    if (intersectSphere(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }
    }

    m = translate(vec3(-0.6, 0.4, 0.0));
    im = invTranslate(vec3(-0.6, 0.4, 0.0));
    m = m*rotateX(0.2*iTime);
    im = invRotateX(0.2*iTime)*im;
    m = m*rotateY(-0.3*iTime);
    im = invRotateY(-0.3*iTime)*im;
    m = m*scale(0.5);
    im = invScale(0.5)*im;
    new_p = (im*vec4(p, 1.0)).xyz;
    new_d = (im*vec4(d, 0.0)).xyz;
    if (intersectCube(new_near, new_normal, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

    m = translate(vec3(-0.6, -0.4, 0.0));
    im = invTranslate(vec3(-0.6, -0.4, 0.0));
    m = m*rotateY(0.5*iTime);
    im = invRotateY(0.5*iTime)*im;
    m = m*rotateX(0.6*iTime);
    im = invRotateX(0.6*iTime)*im;
    m = m*scale(0.5);
    im = invScale(0.5)*im;
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

    vec3 p = vec3(0.0, 0.0, -4.0+1.5*sin(0.2*iTime));
    vec3 d = vec3(0.5*uv, 1.0);
    float seed = uv.x+uv.y;
    seed = rand(seed)+uv.y-uv.x;
    seed = rand(seed)+iTime;
    seed = rand(seed);

    vec3 normal;
    float near;
    if (intersectScene(near, normal, p, d, true)) {
        p = p+near*d;
        vec3 color = vec3(0.0, 0.0, 0.0);
        float gain = 1.0/float(64);
        normal = normalize(normal);
        for (int i = 0; i < 64; ++i) {
            vec3 secondary = vectorOutOfPlane(seed, normal);
            //secondary = normal;
            vec3 new_p = p+0.0001*secondary;
            vec3 ignore;
            float ignore2;
            bool did = intersectScene(ignore2, ignore, new_p, secondary, true);
            if (did) {
                color += vec3(0.0, 0.0, 0.0);
            } else {
                color += vec3(0.8, 0.8, 0.8);
            }
        }
        fragColor = vec4(gain*color, 1.0);
        //fragColor = vec4(normal, 1.0);
    } else {
        fragColor = vec4(0.1, 0.1, 0.1, 1.0);
    }
}
