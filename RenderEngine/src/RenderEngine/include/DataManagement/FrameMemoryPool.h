
#ifndef _FRAMEMEMORYPOOL_H
#define _FRAMEMEMORYPOOL_H

#include "Prerequisites.h"

#include <vector>

DECLARE_ENGINE_NAMESPACE

#define ALIGNUP(nAddress, nBytes) ( (((uint32_t)nAddress)+ (nBytes)-1) & (~((nBytes)-1)) )

// ONE of these 2 should be enabled
#define USE_UPPER_HEAP 1 
#define USE_LOWER_HEAP 0
/*
        Memory Block
             |
             v
|---------------------------| Cap
|  Your own allocated mem.  |
| ========================= | Upper Heap Pointer(Goes down as u allocate mem)
|                           |
|                           |
|                           |
|                           |
|                           |
|                           |
|                           |
|                           |
| ========================= | Lower Heap Pointer(Goes up as u allocate mem)
| Your own allocated mem.   |
|---------------------------| Base
*/
enum HEAP_LOCATION { LOWER_HEAP = 0, UPPER_HEAP, MAX_HEAP };

typedef struct {
	uint8_t* pFrame;
	uint32_t size;
} Frame_t;

enum MMGR_OPTIMIZE_MODE
{
	MMGR_OPTIMIZE_FRAGMENTATION = 0,	//Best when you are doing a lot of deallocation and allocation, but more speed required
	MMGR_OPTIMIZE_SPEED = 1,			//Best when you are doing a lot of small-size allocations, but more wastage of memory
	MMGR_OPTIMIZE_MAX = 2
};

/** \ingroup ResourceManagement
 * @class FrameMemoryPool
 *
 * Learnt from Game Programming Gems 1!
 * Frame memory pool that allocates unknown sizes of memory, using a 'frame' to mark sections of stored info.
 * Can be optimized differently.
 * TODO: Optimize new-ing Frame_t?
 * 
 * \Header
 */
class FrameMemoryPool
{
public:
	/**
	 * Allocates a specified amount of memory.
	 * @param sizeinbytes The size of the pool in bytes
	 * @param byte_alignment The boundary where data is stored, must be a power of 2
	 * @note called by this class' constructor
	 */
	bool Init(uint32_t sizeinbytes, uint32_t byte_alignment);
	/**
	 * Called by destructor.
	 * Frees the memory that was allocated in Init().
	 */
	void Shutdown();

	/**
	 * Gets a memory block of the given size in bytes.
	 * @param bytes The size of the memory to allocate
	 * @return The base address of the memory block, 0 if there is insufficient memory.
	 */
	void* AllocFrameMemory(uint32_t bytes);
	/**
	 * Frees a memory block that was allocated using this class.
	 * @param ptr The memory block to be return back to the pool.
	 */
	void DeAllocFrameMemory(void *ptr);

	/**
	 * Returns a frame handle which can be used to later release memory allocated after calling the function.
	 * @note Recommended if you want to release memory for MMGR_OPTIMIZE_SPEED.
	 */
	Frame_t SetFrame();
	/**
	 * Sets the Upper/Lower Frame pointer to the specified frame's pointer.
	 * @note Recommended if you want to release memory for MMGR_OPTIMIZE_SPEED.
	 */
	void ReleaseFrame(Frame_t frame);

	FrameMemoryPool(MMGR_OPTIMIZE_MODE _mode=MMGR_OPTIMIZE_FRAGMENTATION, uint32_t size=1000000, uint32_t bytealignment=8);
	~FrameMemoryPool();

private:
	uint32_t m_ByteAlignment;	// Memory Alignment in Bytes should be in power of 2.

	uint8_t *m_MemoryBlock;	// Value return by new/malloc
	uint8_t *m_BasePtr;	// Base pointer
	uint8_t *m_CapPtr;	// Cap pointer

	uint8_t *m_FramePtr[MAX_HEAP];	// [LOWER_HEAP]Lower frame and [UPPER_HEAP]Upper frame pointer

	std::vector<Frame_t*> m_FramesInUse;
	std::vector<Frame_t*> m_FreeFrames;

	Frame_t *temp_frame; // for copying data
	//FreeListPool<Frame_t>* m_FramePool;

	MMGR_OPTIMIZE_MODE mode;

};

END_ENGINE_NAMESPACE

#endif