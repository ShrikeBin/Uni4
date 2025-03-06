// Value of type T
struct Node<T> 
{
    value: T,
    next: Option<Box<Node<T>>>, // enum (some value T or nullptr) (Box is a smartptr)
}

// Define Queue
struct Queue<T> 
{
    head: Option<Box<Node<T>>>, // Head of the linked list
}

// implement Queue methods
impl<T> Queue<T> 
{
    fn new() -> Self 
    {
        Queue { head: None }
    }

    // self: This means the method takes ownership of the struct instance (moves it).
    // &self: This means the method borrows the struct instance immutably (doesn't allow modification).
    // &mut self: This means the method borrows the struct instance mutably (allows modification).
    // that is why we can just call push(Value T)

    fn push(&mut self, value: T) 
    {
        let new_node = Box::new(Node 
        {
            value,
            next: self.head.take(), // take() "takes" value out of Option and leaves it as null (literally takes the thing out of it xd)
        });
        self.head = Some(new_node); 
    }

    fn pop(&mut self) -> Option<T> 
    {
        self.head.take().map(|node| // for safety map if theres Something as Node (if theres None it wont do this method)
        {
            self.head = node.next;
            return node.value;
        })
    }
}

fn main() 
{
    let mut queue = Queue::new(); // Create a Queue

    for i in 0..50
    {
        queue.push(i);
    }

    for i in 0..60
    {
        if let Some(value) = queue.pop() 
        {
            println!("{}", value);
        } 
        else // if Some is Nullptr
        {
            println!("Empty Queue for i: {}", i);
        }
    }
}
