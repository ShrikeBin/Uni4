import java.util.ArrayList;
import java.util.List;

public class SplayTree implements TREE
{
    private class SplayNode
    {
        public int val;
        public SplayNode left;
        public SplayNode right;
        public SplayNode p;

        SplayNode(int data) { val = data; left = right = p = null;}

        public boolean isOnLeft() 
        {
            if (metrics != null) metrics.pointerReads++;
            if(p != null) return this == p.left;
            return true;
        }
    }

    private SplayNode root = null;
    private Metrics metrics;

    SplayTree(int data){addNode(data);}
    SplayTree(){};

    @Override
    public void setMetrics(Metrics m) {this.metrics = m;}
    @Override
    public Metrics getMetrics() {return this.metrics;}

    @Override
    public void addNode(int data)
    {
        insert(data);
    }
    
    @Override
    public void deleteNode(int data)
    {
        delete(data);
    }

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

    private void insert(int key) 
    {
        SplayNode z = root;
        SplayNode p = null;
    
        while (z != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            p = z;
            if (metrics != null) metrics.comparisons++;
            if (key < z.val) 
            {
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerWrites++;
                z = z.left;
            } 
            else if (key > z.val) 
            {
                if (metrics != null) metrics.comparisons++;
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerWrites++;
                z = z.right;
            } 
            else 
            {
                if (metrics != null) metrics.comparisons++;
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerReads++;
                return;
            }
        }
    
        z = new SplayNode(key);
        if (metrics != null) metrics.pointerWrites++;
        z.p = p;
    
        if (p == null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            root = z;
        } 
        else if (key < p.val) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            p.left = z;
        } 
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            p.right = z;
        }
    
        splay(z);
    }
    

    private void delete(int key) 
    {
        SplayNode z = find(key);
        if (z == null) return;
    
        splay(z);
    
        if (z.left == null) 
        {
            if (metrics != null) metrics.pointerReads++;
            replace(z, z.right);
        } 
        else if (z.right == null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            replace(z, z.left);
        } 
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            SplayNode y = subtreeMinimum(z.right);
            if (metrics != null) metrics.pointerReads++;
            if (y.p != z) 
            {
                replace(y, y.right);
                if (metrics != null) metrics.pointerWrites++;
                y.right = z.right;
                if (metrics != null) metrics.pointerReads++;
                if (y.right != null)
                {
                    if (metrics != null) metrics.pointerWrites++;
                   y.right.p = y;
                }
            }
            replace(z, y);
            if (metrics != null) metrics.pointerWrites++;
            y.left = z.left;
            if (metrics != null) metrics.pointerReads++;
            if (y.left != null) 
            {
                y.left.p = y;
            }
        }
        if (metrics != null) metrics.pointerWrites++;
        root.p = null;
    }   

    private SplayNode find(int key) 
    {
        SplayNode z = root;
        while (z != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (key < z.val) 
            {
                if (metrics != null) metrics.pointerWrites++;
                z = z.left;
            } 
            else if (key > z.val) 
            {
                if (metrics != null) metrics.pointerReads++;
                if (metrics != null) metrics.pointerWrites++;
                z = z.right;
            } 
            else 
            {
                if (metrics != null) metrics.pointerReads++;
                return z;
            }
        }
        return null;
    }    

    private void splay(SplayNode x) 
    {
        while (x.p != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (x.p.p == null) 
            {
                // Zig case
                if (x.isOnLeft()) 
                {
                    rightRotate(x.p);
                } else 
                {
                    leftRotate(x.p);
                }
            } 
            else if (x.isOnLeft() && x.p.p.left == x.p) 
            {
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads++;
                // Zig-Zig (left-left)
                rightRotate(x.p.p);
                rightRotate(x.p);
            } 
            else if (!x.isOnLeft() && x.p.p.right == x.p) 
            {
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads+=3;
                // Zig-Zig (right-right)
                leftRotate(x.p.p);
                leftRotate(x.p);
            } 
            else if (x.isOnLeft() && x.p.p.right == x.p) 
            {
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads++;
                // Zig-Zag (left-right)
                rightRotate(x.p);
                leftRotate(x.p);
            } 
            else 
            {
                
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads+=3;
                if (metrics != null) metrics.pointerReads++;
                // Zig-Zag (right-left)
                leftRotate(x.p);
                rightRotate(x.p);
            }
        }
    }
    
    private void replace(SplayNode u, SplayNode v) 
    {
        if (u.p == null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            root = v;
        } 
        else if (u == u.p.left) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            u.p.left = v;
        } 
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            u.p.right = v;
        }
        if (metrics != null) metrics.pointerReads++;
        if (v != null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            v.p = u.p;
        }
    }
    
    private SplayNode subtreeMinimum(SplayNode u) 
    {
        while (u.left != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            u = u.left;
        }
        return u;
    }
    
    private void leftRotate(SplayNode x) 
    {
        SplayNode y = x.right;
        if (metrics != null) metrics.pointerReads++;
        if (y != null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            x.right = y.left;
            if (metrics != null) metrics.pointerReads++;
            if (y.left != null) 
            {
                if (metrics != null) metrics.pointerWrites++;
                y.left.p = x;
            }
            if (metrics != null) metrics.pointerWrites++;
            y.p = x.p;
        }
        
        if (metrics != null) metrics.pointerReads++;
        if (x.p == null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            root = y;
        } 
        else if (x == x.p.left) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            x.p.left = y;
        } 
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            x.p.right = y;
        }
        
        if (metrics != null) metrics.pointerReads++;
        if (y != null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            y.left = x;
        }
        x.p = y;
    }
    
    private void rightRotate(SplayNode x) 
    {
        SplayNode y = x.left;
        if (y != null) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            x.left = y.right;
            if (metrics != null) metrics.pointerReads++;
            if (y.right != null) 
            {
                if (metrics != null) metrics.pointerWrites++;
                y.right.p = x;
            }
            if (metrics != null) metrics.pointerWrites++;
            y.p = x.p;
        }
        
        if (metrics != null) metrics.pointerReads++;
        if (x.p == null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            root = y;
        } 
        else if (x == x.p.left) 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            x.p.left = y;
        } 
        else 
        {
            if (metrics != null) metrics.pointerReads++;
            if (metrics != null) metrics.pointerWrites++;
            x.p.right = y;
        }
        
        if (metrics != null) metrics.pointerReads++;
        if (y != null) 
        {
            if (metrics != null) metrics.pointerWrites++;
            y.right = x;
        }
        if (metrics != null) metrics.pointerWrites++;
        x.p = y;
    }

    private int heightRecursive(SplayNode node) 
    {
        if (node == null) return 0;
        return 1 + Math.max(heightRecursive(node.left), heightRecursive(node.right));
    }

    private void inOrderTraversal(SplayNode node) 
    {
        if (node == null) return;
        inOrderTraversal(node.left);
        System.out.print(node.val + " ");
        inOrderTraversal(node.right);
    }

    private void printRec(SplayNode node, String prefix, boolean isTail, boolean isLeft, boolean isFirst, List<String> lines) 
    {
        if (node == null) return;

        if(!isFirst)
        {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + node.val);
        }
        else
        {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + node.val);
        }
        List<SplayNode> children = new ArrayList<>();
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

