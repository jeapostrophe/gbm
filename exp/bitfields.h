#define BIT(n)                  ( 1<<(n) )
#define BIT_MASK(len)           ( BIT(len)-1 )

#define BIT_SET(y, mask)        ( y |=  (mask) )
#define BIT_CLEAR(y, mask)      ( y &= ~(mask) )
#define BIT_FLIP(y, mask)       ( y ^=  (mask) )

#define BF_MASK(start, len)     ( BIT_MASK(len)<<(start) )
#define BF_PREP(x, start, len)  ( ((x)&BIT_MASK(len)) << (start) )
#define BF_GET(y, start, len)   ( ((y)>>(start)) & BIT_MASK(len) )
#define BF_SET(y, x, start, len)    \
    ( y= ((y) &~ BF_MASK(start, len)) | BF_PREP(x, start, len) )

static inline uint32_t bf_get(uint32_t y, uint32_t shift, uint32_t len) {    
    return (y>>shift) & BIT_MASK(len);
}
