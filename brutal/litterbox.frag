vec4 cube[6] = vec4[](
    vec4(0.0, 0.0, -1.0, -1.0),      // -z-1 < 0 i.e. z > -1
    vec4(0.0, 0.0, 1.0, -1.0),       // 
    vec4(-1.0, 0.0, 0.0, -1.0),      // 
    vec4(1.0, 0.0, 0.0, -1.0),
    vec4(0.0, -1.0, 0.0, -1.0),
    vec4(0.0, 1.0, 0.0, -1.0));     // 

float rand(inout float seed) {
    seed = seed+0.63;
    float s = 1000.0*(seed+cos(17.0*seed-22.1*seed*seed));
    seed = 11.37*seed+1.2;
    seed = seed-floor(seed);
    return s-floor(s);
}

float pi = 3.1415926;

vec3 pointOrthoToPlane(inout float seed, vec3 n) {
    float z = -1.0+2.0*rand(seed);
    float rxy = sqrt(1 - z*z);
    float phi = 2.0*pi*rand(seed);
    float x = rxy * cos(phi);
    float y = rxy * sin(phi);
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

bool intersectConvex(out float near, out vec3 normal, vec4 cube[6], vec3 p, vec3 d) {
    near = -1000.0;
    float far = 1000.0;
    for (int i = 0; i < 6; ++i) {
        vec4 n = cube[i];
        float np = dot(n, vec4(p, 1.0)); // 4
        float nd = dot(n, vec4(d, 0.0)); // -1
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
    }
    if (near > 0.0 && near < far) {
        return true;
    } else {
        return false;
    }
}

bool intersectScene(out float near, out vec3 normal, vec4 cube[6], vec3 p, vec3 d) {
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
    if (intersectConvex(new_near, new_normal, cube, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
        }
    }

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
    if (intersectConvex(new_near, new_normal, cube, new_p, new_d)) {
        did = true;
        if (new_near < near) {
            normal = (m*vec4(new_normal, 0.0)).xyz;
            near = new_near;
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
    if (intersectConvex(new_near, new_normal, cube, new_p, new_d)) {
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
    if (intersectConvex(new_near, new_normal, cube, new_p, new_d)) {
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

    vec3 p = vec3(0.0, 0.0, -4.0);
    vec3 d = vec3(0.5*uv, 1.0);
    float seed = uv.x+uv.y;
    seed = rand(seed)+uv.y-uv.x;
    seed = rand(seed)+iTime;
    seed = rand(seed);

    vec3 normal;
    float near;
    if (intersectScene(near, normal, cube, p, d)) {
        p = p+near*d;
        vec3 color = vec3(0.0, 0.0, 0.0);
        float gain = 1.0/float(64);
        for (int i = 0; i < 64; ++i) {
            vec3 secondary = pointOrthoToPlane(seed, normal);
            vec3 new_p = p+0.00001*secondary;
            vec3 ignore;
            float ignore2;
            bool did = intersectScene(ignore2, ignore, cube, new_p, secondary);
            if (did) {
                color += vec3(0.0, 0.0, 0.0);
            } else {
                color += vec3(0.8, 0.8, 0.8);
            }
        }
        fragColor = vec4(gain*color, 1.0);
    } else {
        fragColor = vec4(0, 0, 0, 1.0);
    }
}
