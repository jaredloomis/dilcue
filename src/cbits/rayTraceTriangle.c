#define EPSILON 0.000001

#define CROSS(dest,v1,v2) dest[0]=v1[1]*v2[2]-v1[2]*v2[1]; dest[1]=v1[2]*v2[0]-v1[0]*v2[2]; dest[2]=v1[0]*v2[1]-v1[1]*v2[0];
#define DOT(v1,v2) (v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])
#define SUB(dest,v1,v2) dest[0]=v1[0]-v2[0]; dest[1]=v1[1]-v2[1]; dest[2]=v1[2]-v2[2];

static int intersect_triangle2(
    float orig[3],  float dir[3],
    float vert0[3], float vert1[3], float vert2[3],
    float* t,       float* u,       float* v)
{
    float e1[3], e2[3], s1[3], s2[3], d[3];
    float divisor, inv_divisor, b1, b2, tt;

    SUB(e1, vert1, vert0);
    SUB(e2, vert2, vert0);

    CROSS(s1, dir, e2);

    divisor = DOT(s1, e1);

   if(divisor > -EPSILON && divisor < EPSILON)
        return 0;

   inv_divisor = 1 / divisor;

   SUB(d, orig, vert0);

    b1 = DOT(d, s1) * inv_divisor;

    if(b1 < 0 || b1 > 1)
        return 0;

    CROSS(s2, d, e1);

    b2 = DOT(dir, s2) * inv_divisor;

    if(b2 < 0 || b1 + b2 > 1)
        return 0;

    tt = DOT(e2, s2) * inv_divisor;

    if(tt < 0)
        return 0;

    return 1;
}

static int intersect_triangle(
    float orig[3],  float dir[3],
    float vert0[3], float vert1[3], float vert2[3],
    float* t,       float* u,       float* v)
{
   float edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
   float det, inv_det;

   /* find vectors for two edges sharing vert0 */
   SUB(edge1, vert1, vert0);
   SUB(edge2, vert2, vert0);

   /* begin calculating determinant - also used to calculate U parameter */
   CROSS(pvec, dir, edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = DOT(edge1, pvec);

#ifdef TEST_CULL           /* define TEST_CULL if culling is desired */
   if (det < EPSILON)
      return 0;

   /* calculate distance from vert0 to ray origin */
   SUB(tvec, orig, vert0);

   /* calculate U parameter and test bounds */
   *u = DOT(tvec, pvec);
   if (*u < 0.0 || *u > det)
      return 0;

   /* prepare to test V parameter */
   CROSS(qvec, tvec, edge1);

    /* calculate V parameter and test bounds */
   *v = DOT(dir, qvec);
   if (*v < 0.0 || *u + *v > det)
      return 0;

   /* calculate t, scale parameters, ray intersects triangle */
   *t = DOT(edge2, qvec);
   inv_det = 1.0 / det;
   *t *= inv_det;
   *u *= inv_det;
   *v *= inv_det;
#else                    /* the non-culling branch */
   if (det > -EPSILON && det < EPSILON)
     return 0;
   inv_det = 1.0 / det;

   /* calculate distance from vert0 to ray origin */
   SUB(tvec, orig, vert0);

   /* calculate U parameter and test bounds */
   *u = DOT(tvec, pvec) * inv_det;
   if (*u < 0.0 || *u > 1.0)
     return 0;

   /* prepare to test V parameter */
   CROSS(qvec, tvec, edge1);

   /* calculate V parameter and test bounds */
   *v = DOT(dir, qvec) * inv_det;
   if (*v < 0.0 || *u + *v > 1.0)
     return 0;

   /* calculate t, ray intersects triangle */
   *t = DOT(edge2, qvec) * inv_det;
#endif
   return *t > 0;
}

int rayTraceTriangleC(float ox, float oy, float oz,
                      float dx, float dy, float dz,
                      float x1, float y1, float z1,
                      float x2, float y2, float z2,
                      float x3, float y3, float z3,
                      float* t, float* u, float* v) {
    float orig[3]  = {ox, oy, oz};
    float dir[3]   = {dx, dy, dz};
    float vert1[3] = {x1, y1, z1};
    float vert2[3] = {x2, y2, z1};
    float vert3[3] = {x3, y3, z3};

    return intersect_triangle2(orig, dir, vert1, vert2, vert3, t, u, v);
}
