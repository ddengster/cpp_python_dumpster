
#include "DataManagement/FrameMemoryPool.h"

DECLARE_ENGINE_NAMESPACE

FrameMemoryPool::FrameMemoryPool(MMGR_OPTIMIZE_MODE _mode, uint32_t size, uint32_t bytealignment)
:mode(_mode)
{
	Init(size, bytealignment);
	/* Takes a lot of time!*/
	//m_FramePool = new FreeListPool<Frame_t>(1000000);
}

FrameMemoryPool::~FrameMemoryPool()
{	
	uint32_t i=0;
	for (i=0; i<m_FramesInUse.size(); ++i)
	{
		if (m_FramesInUse[i])
			delete m_FramesInUse[i];
		m_FramesInUse[i] = 0;
	}
	m_FramesInUse.clear();
	for (i=0; i<m_FreeFrames.size(); ++i)
	{
		if (m_FreeFrames[i])
			delete m_FreeFrames[i];
		m_FreeFrames[i] = 0;
	}
	m_FreeFrames.clear();

	Shutdown();
}

bool FrameMemoryPool::Init(uint32_t sizeinbytes, uint32_t byte_alignment)
{
	//Make sure size in bytes is a sample of ByteAlignment
	sizeinbytes = ALIGNUP(sizeinbytes, byte_alignment);
	
	m_MemoryBlock = (uint8_t*)malloc(sizeinbytes + byte_alignment);

	if (m_MemoryBlock == 0)
	{
		//error: not enough memory
		assert(0 && "Not enough memory to init frame memory system!" );
		return false;
	}

	m_ByteAlignment = byte_alignment;

	//Set up base pointer
	m_BasePtr = (uint8_t*)ALIGNUP(m_MemoryBlock, byte_alignment);
	//Set up Cap pointer
	m_CapPtr = (uint8_t*)ALIGNUP(m_MemoryBlock+sizeinbytes, byte_alignment);
	
	//Initialize lower and upper frame pointers
	m_FramePtr[LOWER_HEAP] = m_BasePtr;
	m_FramePtr[UPPER_HEAP] = m_CapPtr;

	return true;
}

void FrameMemoryPool::Shutdown()
{
	free(m_MemoryBlock);
}

void* FrameMemoryPool::AllocFrameMemory(uint32_t bytes)
{
	uint8_t *memory = 0;

	bytes = ALIGNUP(bytes, m_ByteAlignment);

	if (!mode)//If you optimize this memory system for anti fragmentation
	{
		//check if we can reuse memory
		uint32_t i=0;
		for (i=0; i<m_FreeFrames.size(); ++i)
		{
			//we found a frame that is equal size
			if (m_FreeFrames[i]->size == bytes)
			{
				m_FramesInUse.push_back(m_FreeFrames[i]);
				memory = m_FreeFrames[i]->pFrame;
				m_FreeFrames.erase(m_FreeFrames.begin()+i);
				//std::cout << "Free frame found.\n";
				return memory;
			}
		}
	}

	//check for available memory
	if (m_FramePtr[LOWER_HEAP]+bytes > m_FramePtr[UPPER_HEAP])
	{
		assert(0 && "Not enough memory!");
		return 0;
	}

	//now perform memory allocation
#if USE_UPPER_HEAP
		//move the upper frame pointer down 1st, then make memory point to the upper frame pointer.
		/*
		|------------| Original upper frame ptr
		|            | -> bytes for memory to use
		|            | -> bytes for memory to use
		|------------| Now:Upper Frame Ptr <- memory
		*/		

		m_FramePtr[UPPER_HEAP] -= bytes;
		memory = m_FramePtr[UPPER_HEAP];

		//create frame and store in the vector
		if (!mode) //If you optimize this memory system for anti fragmentation
		{
			//temp_frame = m_FramePool->NewInstance(); // 25% faster than normal new
			temp_frame = new Frame_t();
			temp_frame->pFrame = memory;
			temp_frame->size = bytes;
			m_FramesInUse.push_back(temp_frame);
		}
#elif USE_LOWER_HEAP
		//set memory to lower frame ptr 1st, then move the lower frame pointer up.(thus setting the limit for memory?)
		/*
		|------------| Now:lower frame ptr 
		|            | -> bytes for memory to use
		|            | -> bytes for memory to use
		|------------| Original Lower Frame Ptr <- memory
		*/		

		memory = m_FramePtr[LOWER_HEAP];
		m_FramePtr[LOWER_HEAP] +=  bytes;

		if (!mode) //If you optimize this memory system for anti fragmentation
		{
			temp_frame = new Frame_t();
			temp_frame->pFrame = memory;
			temp_frame->size = bytes;
			m_FramesInUse.push_back(temp_frame);
		}
	}
#endif

	return (void*)memory;
}

void FrameMemoryPool::DeAllocFrameMemory(void* ptr)
{
	if (!mode) //If you optimize this memory system for anti fragmentation
	{
		uint32_t i=0;
		for (i=0; i<m_FramesInUse.size(); ++i)
		{
			if (m_FramesInUse[i]->pFrame == ptr)
			{
				//mark them as free
				m_FreeFrames.push_back(m_FramesInUse[i]);
				m_FramesInUse.erase(m_FramesInUse.begin()+i);
			}
		}
	}
}


Frame_t FrameMemoryPool::SetFrame()
{
	Frame_t frame;
#if USE_UPPER_HEAP
	//point to the lower/upper frame
	frame.pFrame = m_FramePtr[UPPER_HEAP];
#elif USE_LOWER_HEAP
	//point to the lower/upper frame
	frame.pFrame = m_FramePtr[LOWER_HEAP];
#endif
	return frame;
}

void FrameMemoryPool::ReleaseFrame(Frame_t frame)
{
	//assert if pFrame is lower than lower heap or higher than upper heap
	assert( (uint32_t)frame.pFrame<=(uint32_t)m_FramePtr[LOWER_HEAP]);

	assert( (uint32_t)frame.pFrame>=(uint32_t)m_FramePtr[UPPER_HEAP]);
	//sets the upper/lower frame pointer to the frame, therefore 'erasing' all the memory that was occupied before.
#if USE_UPPER_HEAP
	m_FramePtr[UPPER_HEAP] = frame.pFrame;
#elif USE_LOWER_HEAP
	m_FramePtr[LOWER_HEAP] = frame.pFrame;
#endif
}


END_ENGINE_NAMESPACE