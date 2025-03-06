#ifndef
#define QUEUE.HPP

#include <iostream>

// Node structure for linked list
template <typename T>
struct Node 
{
    T data;
    Node* next;
};

// Templated Queue class
template <typename T>
class Queue 
{
private:
    Node<T>* front;
    Node<T>* rear;

public:
    // Constructor
    Queue() 
    {
        front = rear = nullptr;
    }

    // Enqueue operation (insert at the rear)
    void enqueue(T value) 
    {
        Node<T>* newNode = new Node<T>;
        newNode->data = value;
        newNode->next = nullptr;

        if (rear == nullptr) 
        {
            front = rear = newNode;  // Queue is empty, new node is both front and rear
            return;
        }

        rear->next = newNode;  // Add new node to the end
        rear = newNode;        // Move rear to the new node
    }

    // Dequeue operation (remove from the front)
    T dequeue() 
    {
        if (front == nullptr) 
        {
            cout << "Queue is empty!" << endl;
            return T();  // Return default-constructed value
        }

        Node<T>* temp = front;
        T value = temp->data;
        front = front->next;

        if (front == nullptr) 
        {
            rear = nullptr;  // If the queue is empty after dequeue, set rear to nullptr
        }

        delete temp;  // Free the memory of the old front node
        return value;
    }

    // Check if the queue is empty
    bool isEmpty() const 
    {
        return front == nullptr;
    }

    // Peek operation (get the front element without removing it)
    T peek() const 
    {
        if (front == nullptr) {
            cout << "Queue is empty!" << endl;
            return T();
        }
        return front->data;
    }

    // Destructor to free memory
    ~Queue() 
    {
        while (front != nullptr) {
            dequeue();
        }
    }
};







#endif