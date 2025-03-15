import java.util.*;

// Головний клас для запуску програми
public class Main {
    public static void main(String[] args) {
        // Створюємо нове бінарне дерево пошуку
        BinarySearchTree tree = new BinarySearchTree();
        
        // Вставляємо елементи в дерево
        tree.insert(50);
        tree.insert(30);
        tree.insert(20);
        tree.insert(40);
        tree.insert(70);
        tree.insert(60);
        tree.insert(80);
        
        // Виводимо порядок елементів дерева через обхід Inorder
        System.out.println("Inorder traversal:");
        tree.inorder();
        
        // Перевіряємо наявність елементів у дереві
        System.out.println("Search 20: " + tree.search(20));  // Має знайти елемент 20
        System.out.println("Search 90: " + tree.search(90));  // Має не знайти елемент 90
        
        // Видаляємо елемент 20 з дерева
        tree.delete(20);
        
        // Виводимо порядок елементів дерева після видалення елемента
        System.out.println("Inorder traversal after deletion:");
        tree.inorder();
    }
}

// Клас для представлення вузла бінарного дерева
class Node {
    private int value;           // Значення вузла
    private Node leftChild, rightChild;  // Лівий та правий нащадки

    // Конструктор вузла з заданим значенням
    public Node(int value) {
        this.value = value;
        this.leftChild = null;
        this.rightChild = null;
    }

    // Геттери та сеттери для значення та нащадків
    public int getValue() { return value; }
    public Node getLeftChild() { return leftChild; }
    public Node getRightChild() { return rightChild; }
    public void setLeftChild(Node leftChild) { this.leftChild = leftChild; }
    public void setRightChild(Node rightChild) { this.rightChild = rightChild; }
}

// Клас для реалізації бінарного дерева пошуку
class BinarySearchTree {
    private Node root;  // Корінь дерева

    // Конструктор дерева, спочатку дерево порожнє
    public BinarySearchTree() { this.root = null; }

    // Метод для вставки нового елемента в дерево
    public void insert(int value) {
        if (root == null) {
            root = new Node(value);  // Якщо дерево порожнє, встановлюємо корінь
            return;
        }
        
        Node current = root, parent;
        while (true) {
            parent = current;
            if (value < current.getValue()) {  // Якщо значення менше за поточне, йдемо вліво
                current = current.getLeftChild();
                if (current == null) {
                    parent.setLeftChild(new Node(value));  // Вставляємо новий вузол ліворуч
                    return;
                }
            } else {  // Якщо значення більше або рівне поточному, йдемо праворуч
                current = current.getRightChild();
                if (current == null) {
                    parent.setRightChild(new Node(value));  // Вставляємо новий вузол праворуч
                    return;
                }
            }
        }
    }

    // Метод для пошуку елемента в дереві
    public boolean search(int value) {
        Node current = root;
        while (current != null) {
            if (current.getValue() == value) return true;  // Якщо знайдений елемент, повертаємо true
            current = (value < current.getValue()) ? current.getLeftChild() : current.getRightChild();
        }
        return false;  // Якщо елемент не знайдений, повертаємо false
    }

    // Метод для видалення елемента з дерева
    public boolean delete(int value) {
        Node parent = null, current = root;
        boolean isLeftChild = false;

        // Знаходимо вузол для видалення та його батька
        while (current != null && current.getValue() != value) {
            parent = current;
            if (value < current.getValue()) {
                current = current.getLeftChild();
                isLeftChild = true;  // Вказуємо, що поточний вузол лівий нащадок
            } else {
                current = current.getRightChild();
                isLeftChild = false;  // Вказуємо, що поточний вузол правий нащадок
            }
        }

        if (current == null) return false;  // Якщо не знайдений елемент для видалення

        // Випадок, коли вузол не має нащадків (ліві та праві діти null)
        if (current.getLeftChild() == null && current.getRightChild() == null) {
            if (current == root) root = null;  // Якщо це корінь дерева
            else if (isLeftChild) parent.setLeftChild(null);  // Якщо вузол лівий нащадок
            else parent.setRightChild(null);  // Якщо вузол правий нащадок
        }
        // Випадок, коли вузол має тільки правого нащадка
        else if (current.getLeftChild() == null) {
            if (current == root) root = current.getRightChild();  // Якщо це корінь дерева
            else if (isLeftChild) parent.setLeftChild(current.getRightChild());  // Лівий нащадок
            else parent.setRightChild(current.getRightChild());  // Правий нащадок
        }
        // Випадок, коли вузол має тільки лівого нащадка
        else if (current.getRightChild() == null) {
            if (current == root) root = current.getLeftChild();  // Якщо це корінь дерева
            else if (isLeftChild) parent.setLeftChild(current.getLeftChild());  // Лівий нащадок
            else parent.setRightChild(current.getLeftChild());  // Правий нащадок
        }
        // Випадок, коли вузол має обох нащадків
        else {
            Node successor = getSuccessor(current);  // Знаходимо наступника для заміни
            if (current == root) root = successor;  // Якщо це корінь дерева
            else if (isLeftChild) parent.setLeftChild(successor);  // Лівий нащадок
            else parent.setRightChild(successor);  // Правий нащадок
            successor.setLeftChild(current.getLeftChild());  // Призначаємо лівого нащадка
        }
        return true;
    }

    // Метод для знаходження наступника для видаленого елемента
    private Node getSuccessor(Node deleteNode) {
        Node successorParent = deleteNode, successor = deleteNode;
        Node current = deleteNode.getRightChild();

        // Переміщаємось вліво до самого лівого вузла правого піддерева
        while (current != null) {
            successorParent = successor;
            successor = current;
            current = current.getLeftChild();
        }

        // Якщо наступник не є правим нащадком, переналаштовуємо зв'язки
        if (successor != deleteNode.getRightChild()) {
            successorParent.setLeftChild(successor.getRightChild());
            successor.setRightChild(deleteNode.getRightChild());
        }
        return successor;  // Повертаємо наступника
    }

    // Метод для обходу дерева в порядку Inorder
    public void inorder() {
        inorderRec(root);  // Рекурсивний обхід дерева
        System.out.println();  // Перехід на новий рядок після виведення
    }

    // Рекурсивний метод для Inorder обходу
    private void inorderRec(Node root) {
        if (root != null) {
            inorderRec(root.getLeftChild());  // Спочатку лівий піддерево
            System.out.print(root.getValue() + " ");  // Виводимо значення поточного вузла
            inorderRec(root.getRightChild());  // Потім правий піддерево
        }
    }
}
