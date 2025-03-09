#ifndef CYCLISTD_HPP
#define CYCLISTD_HPP

#include <iostream>

template <typename T>
struct Node 
{
    T data;
    Node* next;
    Node* prev;
};

template <typename T>
class CycListD 
{
public:
    Node<T>* front;
    unsigned long elementcount;

public:
    CycListD() : front(nullptr), elementcount(0) {}

    void insert(T value) 
    {
        Node<T>* newNode = new Node<T>{value, nullptr, nullptr};
        
        if (front == nullptr) 
        {
            front = newNode;
            front->next = front;
            front->prev = front;
        } 
        else 
        {
            newNode->next = front->next;
            newNode->prev = front;
            front->next->prev = newNode;
            front->next = newNode;
            front = newNode;
        }

        elementcount++;
    }

    void merge(CycListD<T>& list) 
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

        Node<T>* temp1 = front->next;
        Node<T>* temp2 = list.front->next;
        
        front->next = temp2;
        temp2->prev = front;
        
        list.front->next = temp1;
        temp1->prev = list.front;
        
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

    ~CycListD() 
    {
        clean();
    }
};

#endif