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

float turb(vec2 uv) {
    float c = 0.0;
    int i;
    float s1 = 2.0;
    float s2 = 1.0;
    float t = 0.0;
    for (i = 0; i < 7; ++i) {
        c += s2*noise(vec3(s1*uv,0.123));
        //c += s2*noise1(s1*uv);
        t += s2;
        s1 *= 2.0;
        s2 *= 1.0;
    }
    return c/t;
}

float concrete(vec2 uv) {
    float d = turb(1.0*uv+vec2(763.1, -123.2));
    float c = 0.1+0.26*smoothstep(-0.4, 0.4, d);

    d = turb(1.0*uv+vec2(1245.0, 12.11));
    c += -0.1*smoothstep(0, 0.1, d);

    //d = turb(40.0*uv+vec2(12.0, 152.11));
    //c += d > 0.2 ? 0.1 : 0.0;

    return 0.5+1.0*c;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy-0.5*iResolution.xy;
    uv += iMouse.xy;
    uv = 2.0*uv/iResolution.y;

    float c = concrete(uv);

    fragColor = vec4(1.01*c, 1.*c, 0.97*c, 1.0);
}


