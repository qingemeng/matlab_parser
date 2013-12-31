package   refactoring.matlab.processing

object StencilTemplate {
  
val JacobiTemplate = """

#include "hemi.h"

#include "global.h"
#include "unroll.h"
#include "inplane-full.h"

#define TYPE<<STINDEX>>		<<STTYPE>>
#define RADIUS<<STINDEX>>	<<STRADIUS>>
#define TILEX<<STINDEX>>	<<STTILEX>>
#define TILEY<<STINDEX>>	<<STTILEY>>
#define REGX<<STINDEX>>		<<STREGX>>
#define REGY<<STINDEX>>		<<STREGY>>
  
typedef InplaneFull< TYPE<<STINDEX>>, RADIUS<<STINDEX>>, TILEX<<STINDEX>>, TILEY<<STINDEX>>, REGX<<STINDEX>>, REGY<<STINDEX>> > Inplane<<STINDEX>>;  
#define INPUT(x,y,z)   (input[((z)+RADIUS<<STINDEX>>) * stride_z + ((y)+RADIUS<<STINDEX>>) * stride_y + (x) + RADIUS<<STINDEX>> + padding])

  
HEMI_KERNEL(<<STNAME>>)
	(TYPE<<STINDEX>>* output,
  	const TYPE<<STINDEX>>* input,
	<<STCOEFFS>>
	const int dimx,
	const int dimy,
	const int dimz) {

	//const int padding = Inplane::padding;
    const int padding = PADDING;
	__shared__ TYPE<<STINDEX>> tile[TILEY<<STINDEX>>*REGY<<STINDEX>> + 2*RADIUS<<STINDEX>>][TILEX<<STINDEX>>*REGX<<STINDEX>> + 2*(RADIUS<<STINDEX>>+padding)];

	//const int     outerDimx    = dimx + 2 * (RADIUS<<STINDEX>> + padding);
	//const int     outerDimy    = dimy + 2 * RADIUS<<STINDEX>>;
    // radius already included
    const int     outerDimx    = dimx + 2*padding;
    const int     outerDimy    = dimy;
	const int     stride_y     = outerDimx;
	const int     stride_z     = stride_y * outerDimy;

    TYPE<<STINDEX>> outbuf[REGY<<STINDEX>>][REGX<<STINDEX>>][RADIUS<<STINDEX>>];
    TYPE<<STINDEX>> behind[REGY<<STINDEX>>][REGX<<STINDEX>>][RADIUS<<STINDEX>>];
    TYPE<<STINDEX>> current[REGY<<STINDEX>>][REGX<<STINDEX>>];

	const int gtidx = (blockIdx.x * blockDim.x + threadIdx.x) * REGX<<STINDEX>>;
	const int gtidy = (blockIdx.y * blockDim.y + threadIdx.y) * REGY<<STINDEX>>;

	// Note grid tile may exceed dimensions: not actual boundary
    bool valid = true;
    if (gtidx >= dimx) valid = false;
    if (gtidy >= dimy) valid = false;
  
	int outputIndex = (gtidy+RADIUS<<STINDEX>>) * stride_y + gtidx + RADIUS<<STINDEX>> + padding;

	Inplane<<STINDEX>> cache(tile, input, stride_y, stride_z);

	// Preload the "infront" and "behind" data
	UNROLLXY16(REGX<<STINDEX>>,REGY<<STINDEX>>, {
		const int gsx = gtidx+rx;
		const int gsy = gtidy+ry;

		for (int i = RADIUS<<STINDEX>> - 2 ; i >= 0 ; i--){
			behind[ry][rx][i] 	= INPUT(gsx,gsy,-i-2);
		}
		current[ry][rx] = INPUT(gsx,gsy,-1);
	});


	__syncthreads();
	for (int iz = 0 ; iz < dimz + RADIUS<<STINDEX>>; iz++){
		UNROLLXY16(REGX<<STINDEX>>,REGY<<STINDEX>>, {
			for (int i = RADIUS<<STINDEX>> - 1 ; i > 0 ; i--)
				behind[ry][rx][i] = behind[ry][rx][i - 1];
			behind[ry][rx][0] = current[ry][rx];
		});
		__syncthreads();

		cache.load();
		__syncthreads();

		int curIndex;
		TYPE<<STINDEX>> value[REGY<<STINDEX>>][REGX<<STINDEX>>];
		UNROLLXY16(REGX<<STINDEX>>,REGY<<STINDEX>>, {
			const int sx = threadIdx.x*REGX<<STINDEX>> + rx + RADIUS<<STINDEX>> + padding;
			const int sy = threadIdx.y*REGY<<STINDEX>> + ry + RADIUS<<STINDEX>>;
			current[ry][rx] = tile[sy][sx];

			//curIndex = outputIndex + ry*stride_y + rx;
			curIndex = ((iz)+RADIUS<<STINDEX>>) * stride_z + ((gtidy+ry)+RADIUS<<STINDEX>>) * stride_y + (gtidx+rx) + RADIUS<<STINDEX>> + padding;
			// expression template here
  			<<STEXPRESSION>>

			outbuf[ry][rx][0] +=  current[ry][rx];

		});
		if(iz >= RADIUS<<STINDEX>> && valid){
			UNROLLXY16(REGX<<STINDEX>>,REGY<<STINDEX>>, {
				output[outputIndex + ry*stride_y + rx] = outbuf[ry][rx][0];
			});
		}
		UNROLLXY16(REGX<<STINDEX>>,REGY<<STINDEX>>, {
			for (int i = 0 ; i < RADIUS<<STINDEX>>-1; i++)
				outbuf[ry][rx][i] = outbuf[ry][rx][i + 1];

			outbuf[ry][rx][RADIUS<<STINDEX>> - 1] = value[ry][rx];
		});
		outputIndex += stride_z;
	}
}
  
  
"""
}