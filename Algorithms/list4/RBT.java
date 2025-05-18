import java.util.ArrayList;
import java.util.List;

class RBT implements TREE 
{
    Node root;
    private Metrics metrics;

    // TODO ADD COUNTING THIS STUFF
    // if (metrics != null) metrics.comparisons++;

    RBT() {root = null;}
    Node getRoot() {return root;}

    private enum COLOR { RED, BLACK }

    private class Node 
    {
        int val;
        COLOR color;
        Node left, right, parent;

        Node(int val) {
            this.val = val;
            parent = left = right = null;
            color = COLOR.RED;
        }
        
        public Node uncle() {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (parent == null || parent.parent == null)
                return null;
            if (parent.isOnLeft())
                return parent.parent.right;
            else
                return parent.parent.left;
        }
        
        public boolean isOnLeft() {
            if (metrics != null) metrics.pointerReads++;
            return this == parent.left;
        }
        
        public Node sibling() {
            if (metrics != null) metrics.pointerReads++;
            if (parent == null)
                return null;
            if (isOnLeft())
            {
                if (metrics != null) metrics.pointerReads++;
                return parent.right;
            }
            if (metrics != null) metrics.pointerReads++;
            return parent.left;
        }
        
        public void moveDown(Node nParent) {
            if (metrics != null) metrics.pointerReads++;
            if (parent != null) {
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerWrites++;
                if (isOnLeft())
                    parent.left = nParent;
                else
                    parent.right = nParent;
            }
            if (metrics != null) metrics.pointerWrites++;
            if (metrics != null) metrics.pointerWrites++;
            nParent.parent = parent;
            parent = nParent;
        }

        public boolean hasRedChild() {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            return (left != null && left.color == COLOR.RED) ||
                    (right != null && right.color == COLOR.RED);
        }
    }

    @Override
    public void setMetrics(Metrics m) {this.metrics = m;}
    @Override
    public Metrics getMetrics() {return this.metrics;}
    
    @Override
    public int getHeight() 
    {
        return heightRecursive(root);
    }

    @Override
    public void printTreeOrder() 
    {
        System.out.print("[START]: ");
        inOrderTraversal(root);
        System.out.println(" [END]");
    }

    @Override
    public void printTreeFull() 
    {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, true, true, lines); // root assumed to be a left node
        for (String line : lines) 
        {
            System.out.println(line);
        }
    }

    // inserts the given value to tree
    @Override
    public void addNode(int n) {
        Node newNode = new Node(n);
        if (root == null) {
            // when root is null
            // simply insert value at root
            newNode.color = COLOR.BLACK;
            if (metrics != null) metrics.pointerWrites++;
            root = newNode;
        } else {
            Node temp = search(n);
            
            // return if value already exists
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.comparisons++;
            if (temp.val == n)
                return;
                
            // if value is not found, search returns the node
            // where the value is to be inserted
 
            // connect new node to correct node
            newNode.parent = temp;
            if (metrics != null) metrics.pointerWrites++;
            if (metrics != null) metrics.comparisons++;
            if (n < temp.val)
            {
                if (metrics != null) metrics.pointerReads++;
                temp.left = newNode;
                if (metrics != null) metrics.pointerWrites++;
            }
            else
            {
                if (metrics != null) metrics.pointerReads++;
                temp.right = newNode;
                if (metrics != null) metrics.pointerWrites++;
            }
            // fix red red violation if exists
            fixRedRed(newNode);
        }
    }

    @Override
    public void deleteNode(int x)
    {
        Node a = search(x);
        removeNode(a);
    }

    // deletes the given node
    private void removeNode(Node v) {
        Node u = BSTreplace(v);
        // True when u and v are both black
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        boolean uvBlack = ((u == null || u.color == COLOR.BLACK) && (v.color == COLOR.BLACK));
        Node parent = v.parent;

        if (u == null) {
            // u is NULL therefore v is leaf
            if (v == root)
            {
                // v is root, making root null
                root = null;
                if (metrics != null) metrics.pointerWrites++;
            }
            else 
            {
                if (uvBlack)
                {
                // u and v both black
                // v is leaf, fix double black at v
                    fixDoubleBlack(v);
                }
                // u or v is red
                else if (v.sibling() != null)
                {
                    // sibling is not null, make it red
                    if (metrics != null) metrics.pointerReads++;
                    v.sibling().color = COLOR.RED;
                    if (metrics != null) metrics.pointerWrites++;
                }
                
                // delete v from the tree
                if (v.isOnLeft())
                {
                    parent.left = null;
                    if (metrics != null) metrics.pointerWrites++;
                }
                else
                {
                    parent.right = null;
                    if (metrics != null) metrics.pointerWrites++;
                }
            }
            return;
        }

        if (v.left == null || v.right == null) {
            // v has 1 child
            if (v == root) {
                // v is root, assign the value of u to v, and delete u
                v.val = u.val;
                if (metrics != null) metrics.pointerWrites++;
                v.left = v.right = null;
                if (metrics != null) metrics.pointerWrites++;
                // delete u;
            } else {
                // Detach v from tree and move u up
                if (v.isOnLeft())
                {
                    if (metrics != null) metrics.pointerWrites++;
                    parent.left = u;
                }
                else
                {
                    if (metrics != null) metrics.pointerWrites++;
                    parent.right = u;
                }

                if (metrics != null) metrics.pointerWrites++;
                u.parent = parent;

                if (uvBlack)
                // u and v both black, fix double black at u
                    fixDoubleBlack(u);
                else
                // u or v red, color u black
                    u.color = COLOR.BLACK;
            }
            return;
        }
        
        // v has 2 children, swap values with successor and recurse
        swapValues(u, v);
        removeNode(u);
    }

    private void fixDoubleBlack(Node x) {
        // Reached root
        if (x == root)
            return;

        Node sibling = x.sibling(), parent = x.parent;

        if (sibling == null)
        // No sibling, double black pushed up
            fixDoubleBlack(parent);
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (sibling.color == COLOR.RED) 
            {
                // sibling red
                parent.color = COLOR.RED;
                sibling.color = COLOR.BLACK;

                if (sibling.isOnLeft())
                    // right case
                    rightRotate(parent);
                else
                    // right case
                    leftRotate(parent);

                fixDoubleBlack(x);
            } 
            else 
            {
                // Sibling black
                if (sibling.hasRedChild()) 
                {
                    // at least 1 red children
                    if (metrics != null) metrics.pointerReads++;
                    if (metrics != null) metrics.pointerReads++;
                    if (sibling.left != null && sibling.left.color == COLOR.RED) 
                    {
                        if (sibling.isOnLeft()) 
                        {
                            // left left
                            if (metrics != null) metrics.pointerWrites++;
                            if (metrics != null) metrics.pointerWrites++;
                            sibling.left.color = sibling.color;
                            sibling.color = parent.color;
                            rightRotate(parent);
                        } else {
                            // right right
                            if (metrics != null) metrics.pointerWrites++;
                            sibling.left.color = parent.color;
                            rightRotate(sibling);
                            leftRotate(parent);
                        }
                    } 
                    else 
                    {
                        if (sibling.isOnLeft()) 
                        {
                            // left right
                            if (metrics != null) metrics.pointerWrites++;
                            sibling.right.color = parent.color;
                            leftRotate(sibling);
                            rightRotate(parent);
                        } else {
                            // right right
                            if (metrics != null) metrics.pointerWrites++;
                            if (metrics != null) metrics.pointerWrites++;
                            sibling.right.color = sibling.color;
                            sibling.color = parent.color;
                            leftRotate(parent);
                        }
                    }
                    parent.color = COLOR.BLACK;
                } 
                else 
                {
                    // 2 black children
                    if (metrics != null) metrics.pointerReads++;
                    if (metrics != null) metrics.pointerReads++;
                    
                    sibling.color = COLOR.RED;
                    if (parent.color == COLOR.BLACK)
                        fixDoubleBlack(parent);
                    else
                        parent.color = COLOR.BLACK;
                }
            }
        }
    }
    
    private void fixRedRed(Node x) {
        
        // if x is root color it black and return
       if (x == root) {
           x.color = COLOR.BLACK;
           return;
       }
       
       // initialize parent, grandparent, uncle
       Node parent = x.parent, grandparent = parent.parent, uncle = x.uncle();

       if (metrics != null) metrics.pointerReads++;

       if (parent.color != COLOR.BLACK) 
       {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;

            if (uncle != null && uncle.color == COLOR.RED) {
                // uncle red, perform recoloring and recurse
                if (metrics != null) metrics.pointerWrites++;
                if (metrics != null) metrics.pointerWrites++;
                if (metrics != null) metrics.pointerWrites++;
                parent.color = COLOR.BLACK;
                uncle.color = COLOR.BLACK;
                grandparent.color = COLOR.RED;
                fixRedRed(grandparent);
            } else {
                // Else perform LR, LL, RL, RR
                if (parent.isOnLeft()) {
                    if (x.isOnLeft())
                        // for left right
                        swapColors(parent, grandparent);
                    else {
                        leftRotate(parent);
                        swapColors(x, grandparent);
                    }
                    // for left left and left right
                    rightRotate(grandparent);
                } else {
                    if (x.isOnLeft()) {
                        // for right left
                        rightRotate(parent);
                        swapColors(x, grandparent);
                    } else
                        swapColors(parent, grandparent);
                        
                    // for right right and right left
                    leftRotate(grandparent);
                }
            }
        }
   }
    
    private Node successor(Node x) {
        Node temp = x;
        if (metrics != null) metrics.pointerReads++;
        while (temp.left != null)
        {
            if (metrics != null) metrics.pointerReads++;
            temp = temp.left;
        }
        return temp;
    }
        
    private void swapColors(Node x1, Node x2) {
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerWrites++;
        if (metrics != null) metrics.pointerWrites++;
        COLOR temp = x1.color;
        x1.color = x2.color;
        x2.color = temp;
    }

    private void swapValues(Node u, Node v) {
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerWrites++;
        if (metrics != null) metrics.pointerWrites++;
        int temp = u.val;
        u.val = v.val;
        v.val = temp;
    }

    private Node search(int n) {
        Node temp = root;
        while (temp != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.comparisons++;
            if (n < temp.val) {
                if (temp.left == null)
                {
                    break;
                }
                else
                {
                    temp = temp.left;
                }
            } else if (n == temp.val) 
            {
                if (metrics != null) metrics.pointerReads++;
                break;
            } 
            else 
            {
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerReads++;
                if (temp.right == null)
                {
                    break;
                }
                else
                {
                    temp = temp.right;
                }
            }
        }

        return temp;
    }
    
    // find node that replaces a deleted node in BST
    private Node BSTreplace(Node x) {
        // when node have 2 children
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        if (x.left != null && x.right != null)
            return successor(x.right);
            
        // when leaf
        if (metrics != null) metrics.pointerReads++;
        if (metrics != null) metrics.pointerReads++;
        if (x.left == null && x.right == null)
            return null;
            
        // when single child
        if (metrics != null) metrics.pointerReads++;
        if (x.left != null)
            return x.left;
        else
            return x.right;
    }

    private void leftRotate(Node x) {
        Node nParent = x.right;
        
        if (metrics != null) metrics.pointerReads++;
        if (x == root)
            root = nParent;
            
        x.moveDown(nParent);

        if (metrics != null) metrics.pointerWrites++;
        x.right = nParent.left;
        if (metrics != null) metrics.pointerReads++;
        if (nParent.left != null)
        {
            if (metrics != null) metrics.pointerWrites++;
            nParent.left.parent = x;
        }
        if (metrics != null) metrics.pointerWrites++;
        nParent.left = x;
    }

    private void rightRotate(Node x) {
        Node nParent = x.left;
        
        if (metrics != null) metrics.pointerReads++;
        if (x == root)
            root = nParent;

        x.moveDown(nParent);

        if (metrics != null) metrics.pointerWrites++;
        x.left = nParent.right;
        if (metrics != null) metrics.pointerReads++;
        if (nParent.right != null)
        {
            if (metrics != null) metrics.pointerWrites++;
            nParent.right.parent = x;
        }
        if (metrics != null) metrics.pointerWrites++;
        nParent.right = x;
    }

    private int heightRecursive(Node node) 
    {
        if (metrics != null) metrics.pointerReads++;
        if (node == null) return 0;
        return 1 + Math.max(heightRecursive(node.left), heightRecursive(node.right));
    }

    private void inOrderTraversal(Node node) 
    {
        if (metrics != null) metrics.pointerReads++;
        if (node == null) return;
        inOrderTraversal(node.left);
        System.out.print(node.val + " ");
        inOrderTraversal(node.right);
    }

    private void printRec(Node node, String prefix, boolean isTail, boolean isLeft, boolean isFirst, List<String> lines) 
    {
        if (node == null || node == null) return;

        if(!isFirst)
        {
            String branchColor = isLeft ? "\u001B[32m" : "\u001B[31m";
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + (node.color == COLOR.RED ? "\u001B[91m" + node.val + "\u001B[0m" : "\u001B[90m" + node.val + "\u001B[0m"));
        }
        else
        {
            String branchColor = isLeft ? "\u001B[32m" : "\u001B[31m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + (node.color == COLOR.RED ? "\u001B[91m" + node.val + "\u001B[0m" : "\u001B[90m" + node.val + "\u001B[0m"));
        }

        List<Node> children = new ArrayList<>();
        List<Boolean> isLeftList = new ArrayList<>();

        if (node.left != null) 
        {
            children.add(node.left);
            isLeftList.add(true);
        }
        if (node.right != null) 
        {
            children.add(node.right);
            isLeftList.add(false);
        }

        for (int i = 0; i < children.size(); i++) 
        {
            boolean childIsTail = (i == children.size() - 1);
            String newPrefix = prefix + (isTail ? "    " : "\u001B[90m" + "│   " + "\u001B[0m");
            printRec(children.get(i), newPrefix, childIsTail, isLeftList.get(i), false, lines);
        }
    }
}