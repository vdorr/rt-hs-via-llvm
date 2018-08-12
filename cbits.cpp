
#include <cstdio>

#include <boost/lockfree/spsc_queue.hpp>
#include <boost/atomic.hpp>

boost::lockfree::spsc_queue<int, boost::lockfree::capacity<1024> > spsc_queue;

extern "C" int cbits_hello (int x) {

//	boost::lockfree::spsc_queue<int>* q = new boost::lockfree::spsc_queue<int>(256);
//	printf("%s%i %i %i %i\n", __FUNCTION__, __LINE__, x, q->read_available(), q->write_available());
	printf("%s:%i %i\n", __FUNCTION__, __LINE__, x);
	return 8;
}

