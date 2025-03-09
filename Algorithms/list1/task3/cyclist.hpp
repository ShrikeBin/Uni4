#ifndef CYCLIST_HPP
#define CYCLIST_HPP

#include <iostream>

template <typename T>
struct Node 
{
    T data;
    Node* next;
};

template <typename T>
class CycList 
{
public:
    Node<T>* front;
    unsigned long elementcount;

public:
    CycList() : front(nullptr), elementcount(0) {}

    void insert(T value) 
    {
        Node<T>* newNode = new Node<T>{value, nullptr};

        if (front == nullptr) 
        {
            front = newNode;
            front->next = front;
        } 
        else 
        {
            newNode->next = front->next;
            front->next = newNode;
            front = newNode;
        }

        elementcount++;
    }

    void merge(CycList<T>& list) 
    {
        if (list.isEmpty()) return;
        if (isEmpty()) 
        {
            front = list.front;
            elementcount = list.elementcount;
            list.front = nullptr;
            list.elementcount = 0;
            return;
        }

        Node<T>* temp = front->next;
        front->next = list.front->next;
        list.front->next = temp;
        front = list.front;

        elementcount += list.elementcount;
        list.front = nullptr;
        list.elementcount = 0;
    }

    bool isEmpty() const 
    {
        return front == nullptr;
    }

    void clean() 
    {
        if (isEmpty()) return;

        Node<T>* current = front->next;
        while (current != front) 
        {
            Node<T>* temp = current;
            current = current->next;
            delete temp;
        }

        delete front;
        front = nullptr;
        elementcount = 0;
    }

    ~CycList() 
    {
        clean();
    }
};

#endif
