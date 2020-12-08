#version 300 es
precision mediump float;

#define ST 0.1
#define MAX_OFFSET 80.

float rand(float co) { return fract(sin(co*(91.3458)) * 47453.5453); }

vec2 uv;
out vec4 fragColor;
// in vec2 gl_FragCoord;
in vec2 vTexCoord;
uniform float frameCount;
uniform vec2 mouse;
uniform vec2 resolution;
uniform sampler2D cam;
uniform int frame;
uniform sampler2D tex0;
uniform sampler2D tex1;
vec2 pixelate ( vec2 pixel, vec2 details ) { return floor(pixel * details) / details; }
float luminance ( vec3 color ) { return (color.r + color.g + color.b) / 3.0; }

const float PI = 3.1415926535897932384626433832795;

vec2 fade(vec2 t) {return t*t*t*(t*(t*6.0-15.0)+10.0);}
vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}

float cnoise(vec2 P){
  vec4 Pi = floor(P.xyxy) + vec4(0.0, 0.0, 1.0, 1.0);
  vec4 Pf = fract(P.xyxy) - vec4(0.0, 0.0, 1.0, 1.0);
  Pi = mod(Pi, 289.0); // To avoid truncation effects in permutation
  vec4 ix = Pi.xzxz;
  vec4 iy = Pi.yyww;
  vec4 fx = Pf.xzxz;
  vec4 fy = Pf.yyww;
  vec4 i = permute(permute(ix) + iy);
  vec4 gx = 2.0 * fract(i * 0.0243902439) - 1.0; // 1/41 = 0.024...
  vec4 gy = abs(gx) - 0.5;
  vec4 tx = floor(gx + 0.5);
  gx = gx - tx;
  vec2 g00 = vec2(gx.x,gy.x);
  vec2 g10 = vec2(gx.y,gy.y);
  vec2 g01 = vec2(gx.z,gy.z);
  vec2 g11 = vec2(gx.w,gy.w);
  vec4 norm = 1.79284291400159 - 0.85373472095314 *
    vec4(dot(g00, g00), dot(g01, g01), dot(g10, g10), dot(g11, g11));
  g00 *= norm.x;
  g01 *= norm.y;
  g10 *= norm.z;
  g11 *= norm.w;
  float n00 = dot(g00, vec2(fx.x, fy.x));
  float n10 = dot(g10, vec2(fx.y, fy.y));
  float n01 = dot(g01, vec2(fx.z, fy.z));
  float n11 = dot(g11, vec2(fx.w, fy.w));
  vec2 fade_xy = fade(Pf.xy);
  vec2 n_x = mix(vec2(n00, n01), vec2(n10, n11), fade_xy.x);
  float n_xy = mix(n_x.x, n_x.y, fade_xy.y);
  return 2.3 * n_xy;
}


void main()
{
    vec2 p = gl_FragCoord.xy/resolution.xy;
    p = 1.0 - p;

	vec4 col = texture(cam, p);
/////////////////////////////////
  // get the webcam as a vec4 using texture2D
  vec4 tex = texture (tex0, p);

  // get the past webcam frame as a texture
  vec4 past = texture(tex1, p);

  // subtract past from tex
  tex.rgb -= past.rgb;
/////////////////////////////////

	// Desaturate
  if(p.x > .25 && p.x < .75 && p.y < .25) { //glass shader  - bed
    if (tex.rgb != past.rgb){
       vec2 texel = 1. / resolution.xy;

       vec4 img = texture(cam, p);

       // you can try and comment / uncomment these three lines
       float step_y = texel.y*(rand(p.x)*MAX_OFFSET) * (sin(sin(frameCount*0.5))*2.0+1.3);	// modulate offset
       //float step_y = texel.y*(rand(uv.x)*100.);										// offset without modulation
       step_y += rand(p.x*p.y*frameCount)*0.01*sin(frameCount); 								// shake offset and modulate it

       if ( dot(img,  vec4(0.299, 0.587, 0.114, 0.) ) > 1.2*(sin(frameCount)*0.325+0.50)){
       	p.y+=step_y;
       } else{
       	p.y-=step_y;
       }

       img = texture(cam, p);
       fragColor = img;
       return;
     }
          }

          // col = vec4(col,1.);



	// Invert
	else if (p.x > .5 && p.y > 0.6) { //purple shader - cosmetic section

      vec4 c = texture(cam,p.xy);
      c = sin(p.x*10.+c*cos(c*6.28+frameCount+p.x)*sin(c+p.y+frameCount)*16.28)*.5+.5;
      c.b+=length(c.rg);
      col = c;

	}
	// Chromatic aberration
	else if (p.x > .75 && p.y < 0.35) { //blue shader - kitchen
    vec2 p = gl_FragCoord.xy/resolution.xy;
    p *= vec2(sin(p.x+frameCount)+3.0);
    p = 1.0 - p;
    vec4 butts;
    float pscale = 6.0;
    if((mod((gl_FragCoord.x+(15.0*sin((gl_FragCoord.y/18.0)+(frameCount)*18.0)))/pscale, 8.0)<1.0)||(mod(gl_FragCoord.y/pscale, 8.0)<1.0)){
        butts = vec4(p.y,0.3,1.0,1.0);
    }
    else{
        float lights=cnoise(p+frameCount*0.00005);
        butts = vec4(lights, 0.5, 1.0, 1.0);
    }
    butts*=texture(cam,mod(p,1.0))*vec4(1.7,1.7,1.7,1.0);
	  col = butts;
	}
	// Color switching
	else if (p.x < .25 && p.y < .75){ //yellow red shader - work area
    // Get normalized texture coordinates
    vec2 p = gl_FragCoord.xy / resolution.xy;
    p = 1.0 - p;

    // Aspect ratio fix
   	p.x -= 0.5;
    p.x *= resolution.x / resolution.y;
    p.x += 0.5;

    // vec4 cam = texture2D(tex0, uv);

    // Pixelate
   	p.xy = pixelate(p.xy, resolution.xy / 4.0);

    // Maths infos about the current pixel position
    vec2 center = p - vec2(0.00000005) + vec2(0.00000001);
    float angle = atan(center.y, center.x);
    float radius = length(center);
    float ratioAngle = (angle / PI) * 0.5 + 0.5;

    // Displacement from noise
    vec2 angleUV = mod(abs(vec2(0, angle / PI)), 1.0);
    // float offset = texture(iChannel1, angleUV).r * 0.5;
    float offset = texture(tex0, angleUV).r * 0.99999;

    // Displaced pixel color
    vec2 q = vec2(cos(angle), sin(angle)) * offset + vec2(0.5);

    // Apply displacement
    p = mix(p, q, step(offset, radius));

    // Get color from texture
    // vec3 color = texture(iChannel0, uv).rgb;
    vec3 color = texture(tex0, p).rgb;
    // Treshold color from luminance
    float lum = luminance(color);
    color = mix(vec3(0), vec3(1,0,0), step(0.45, lum));
    color = mix(color, vec3(1,1,0), step(0.65, lum));
    color = mix(color, vec3(1,1,1), step(0.85, lum));

    // Voila
	   col = vec4(color,1.0);
	}

  else {

    vec4 col = texture(cam, 1.0-p);

    // to not mirror X
    /* color = texture2D(cam, vec2(uv.x,1.0-uv.y)); */

    // Lets just draw the texcoords to the screen
    fragColor = vec4(col.rgb ,1.0);

  }


	//Line

  // if( mod(abs(p.x + .5/resolution.y),.25) < 1./resolution.y )
	// 	col = vec4(1.);

    fragColor = col;
}
