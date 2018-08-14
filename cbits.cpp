
#include <cstdio>

#include <boost/lockfree/spsc_queue.hpp>
//#include <boost/atomic.hpp>

#include <thread>
#include <chrono>

#include <unistd.h>

extern "C" void* rtrt_newQueue ( int cap ) {
	return static_cast<void*>(new boost::lockfree::spsc_queue<int>(cap));
}

extern "C" void rtrt_deleteQueue ( void* q ) {
	delete static_cast<boost::lockfree::spsc_queue<int>*>(q);
}

extern "C" int rtrt_queueReadAvailable ( const void* q ) {
	return static_cast<const boost::lockfree::spsc_queue<int>*>(q)->read_available();
}

extern "C" int rtrt_queueWriteAvailable ( const void* q ) {
	return static_cast<const boost::lockfree::spsc_queue<int>*>(q)->write_available();
}

extern "C" void rtrt_queuePush ( void* q, const int v ) {
	//printf("%s:%i %i\n", __FUNCTION__, __LINE__, v);
	static_cast<boost::lockfree::spsc_queue<int>*>(q)->push(v);
}

extern "C" int rtrt_queuePop ( void* q ) {
	int v = 0;
	static_cast<boost::lockfree::spsc_queue<int>*>(q)->pop(&v);
	return v;
}

extern "C" void rtrt_sleep () {
	usleep(500000);
}

#if 1

boost::lockfree::spsc_queue<int, boost::lockfree::capacity<1024> > spsc_queue;

extern "C" int cbits_hello (int x) {

//	boost::lockfree::spsc_queue<int>* q = new boost::lockfree::spsc_queue<int>(256);
//	printf("%s%i %i %i %i\n", __FUNCTION__, __LINE__, x, q->read_available(), q->write_available());
	printf("%s:%i %i\n", __FUNCTION__, __LINE__, x);
	return 8;
}
#endif

