public class RBTNode 
{
    public int value;
    public RBTNode left;
    public RBTNode right;
    public RBTNode parent; 
    public boolean isRed; // true for red, false for black

    public RBTNode(int input)
    {
        value = input;
        left = right = null;
        isRed = true; // new node is red
    }   
}