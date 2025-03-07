#ifndef QUEUE_HPP
#define QUEUE_HPP

#include <iostream>

template <typename T>
struct Node 
{
    T data;
    Node* next;
};

template <typename T>
class Queue 
{
private:
    Node<T>* front;
    Node<T>* rear;

public:
    Queue() : front(nullptr), rear(nullptr) {}

    void enqueue(T value) 
    {
        Node<T>* newNode = new Node<T>{value, nullptr};

        if (rear == nullptr) 
        {
            front = rear = newNode;
        } 
        else 
        {
            rear->next = newNode;
            rear = newNode;
        }
    }

    T dequeue() 
    {
        if (front == nullptr) 
        {
            // or throw something (i dont really wann do exceptions rn)
            std::cerr << "Queue is empty!" << std::endl;
            return T();
        }

        Node<T>* temp = front;
        T value = temp->data;
        front = front->next;

        if (front == nullptr) 
        {
            rear = nullptr;
        }
        // that cleans 
        delete temp;

        return value;
    }

    bool isEmpty() const 
    {
        return front == nullptr;
    }

    T peek() const 
    {
        if (front == nullptr) 
        {
            // or throw something (i dont really wann do exceptions rn)
            std::cerr << "Queue is empty!" << std::endl;
            return T();
        }
        return front->data;
    }

    ~Queue() 
    {
        while (!isEmpty()) 
        {
            dequeue();
        }
    }
};

#endif
