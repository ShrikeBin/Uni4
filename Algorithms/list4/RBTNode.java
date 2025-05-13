public class RBTNode 
{
    public Integer stem;
    public RBTNode left;
    public RBTNode right;
    public RBTNode parent; 
    public boolean color; // true for red, false for black

    public RBTNode(Integer input) 
    {
        stem = input;
        left = right = null;
        color = true; // new node is red
    }   
}