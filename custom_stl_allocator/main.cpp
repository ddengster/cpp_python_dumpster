#include "Mallocator.h"
#include <vector>
#include <algorithm>
#include <cassert>

// This keeps track of the currently allocated pointers
// Comment this line out and try the next line to see
// why the custom allocator that uses malloc() is required
typedef std::vector<void*, Mallocator<void*> > AllocationList;

// Try this sample with this line INSTEAD
// Our global new will call vector push back
// Which will allocate memory using new
// Which will call our global new... you get the idea
//typedef std::vector<void* > AllocationList;

// NOTE: This global is horribly spectacularly broken
// This is for illustration purposes ONLY
// Please please read this for why:
//    Read 10.12, 10.13, 10.14
//    http://www.parashift.com/c++-faq-lite/ctors.html
AllocationList allocations;

// Very simple and delete overload to
// check for bad pointer deletes
void * operator new(size_t size) throw (std::bad_alloc) {
	void* ptr = malloc(size);

	printf("Allocating 0x%08x\n", (intptr_t)ptr);
	allocations.push_back(ptr);

	return ptr;
}

void operator delete(void * ptr) throw () {
	if (!ptr) { return; }

	printf("Deallocating 0x%08x\n", (intptr_t)ptr);
	AllocationList::iterator I = std::find(allocations.begin(), allocations.end(), ptr);
	assert((I != allocations.end()) && "Bad pointer delete!");
	allocations.erase(I);

	return free(ptr);
}

/* Don't forget the 6 other variations.  new[] and nothrow friends! */

int main() {
	// Simple allocation test
	int* a = new int;
	char* b = new char;
	delete b;
	delete a;
	// A simple double delete
	delete a;
}
