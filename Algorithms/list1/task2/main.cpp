#include "queue.hpp"
#include <iostream>

int main() 
{
    Queue<int> q;

    // Enqueue 50 elements (0 to 49)
    for (int i = 0; i < 50; ++i) 
    {
        q.enqueue(i);
    }

    // Dequeue and print elements
    while (!q.isEmpty()) 
    {
        std::cout << q.dequeue() << std::endl;
    }

    std::cout << q.dequeue();
    // emty queue
    std::cout << std::endl;
    return 0;
}
