class Node:
    def __init__(self, value):
        # Ініціалізуємо вузол дерева з заданим значенням
        self.value = value
        self.left = None  # Лівий нащадок
        self.right = None  # Правий нащадок

class BinarySearchTree:
    def __init__(self):
        # Ініціалізація дерева з порожнім коренем
        self.root = None

    # Вставка елемента в дерево
    def insert(self, value):
        if self.root is None:
            # Якщо дерево порожнє, робимо новий вузол коренем
            self.root = Node(value)
        else:
            # Інакше вставляємо елемент в дерево
            self._insert(self.root, value)

    def _insert(self, node, value):
        # Допоміжний метод для вставки елемента в піддерево
        if value < node.value:
            # Якщо значення менше за поточне, йдемо в лівий підвузол
            if node.left is None:
                node.left = Node(value)  # Якщо лівий підвузол порожній, створюємо новий вузол
            else:
                self._insert(node.left, value)  # Рекурсивно вставляємо в лівий підвузол
        elif value > node.value:
            # Якщо значення більше за поточне, йдемо в правий підвузол
            if node.right is None:
                node.right = Node(value)  # Якщо правий підвузол порожній, створюємо новий вузол
            else:
                self._insert(node.right, value)  # Рекурсивно вставляємо в правий підвузол

    # Пошук елемента в дереві
    def search(self, value):
        return self._search(self.root, value)

    def _search(self, node, value):
        if node is None:
            # Якщо ми досягли кінця дерева, елемент не знайдений
            return False
        if node.value == value:
            # Якщо знайдено вузол з потрібним значенням
            return True
        elif value < node.value:
            # Якщо значення шукаємого елемента менше за поточне, шукаємо в лівому підвузлі
            return self._search(node.left, value)
        else:
            # Якщо значення шукаємого елемента більше за поточне, шукаємо в правому підвузлі
            return self._search(node.right, value)

    # Видалення елемента з дерева
    def delete(self, value):
        self.root = self._delete(self.root, value)

    def _delete(self, node, value):
        if node is None:
            # Якщо вузол порожній, значить елемент не знайдений
            return node

        if value < node.value:
            # Якщо значення шукаємого елемента менше за поточне, шукаємо в лівому підвузлі
            node.left = self._delete(node.left, value)
        elif value > node.value:
            # Якщо значення шукаємого елемента більше за поточне, шукаємо в правому підвузлі
            node.right = self._delete(node.right, value)
        else:
            # Якщо знайдений вузол для видалення
            # Вузол з одним або нульовим нащадком
            if node.left is None:
                return node.right  # Повертаємо правого нащадка, якщо лівий відсутній
            elif node.right is None:
                return node.left  # Повертаємо лівого нащадка, якщо правий відсутній
            
            # Вузол з двома нащадками
            min_larger_node = self._get_min(node.right)  # Знаходимо мінімальний елемент у правому підвузлі
            node.value = min_larger_node.value  # Заміщаємо значення поточного вузла мінімальним елементом
            node.right = self._delete(node.right, min_larger_node.value)  # Видаляємо мінімальний елемент з правого підвузла
        
        return node

    def _get_min(self, node):
        # Допоміжний метод для пошуку мінімального елемента в дереві
        current = node
        while current.left is not None:
            current = current.left  # Переходимо до лівого нащадка, поки не знайдемо мінімальний
        return current

    # Прямий обхід (inorder traversal) дерева
    def inorder(self):
        self._inorder(self.root)
        print()

    def _inorder(self, node):
        if node is not None:
            # Рекурсивно відвідуємо лівий підвузол, виводимо значення вузла та потім правий підвузол
            self._inorder(node.left)
            print(node.value, end=" ")
            self._inorder(node.right)

# Приклад використання
if __name__ == "__main__":
    bst = BinarySearchTree()
    bst.insert(50)
    bst.insert(30)
    bst.insert(20)
    bst.insert(40)
    bst.insert(70)
    bst.insert(60)
    bst.insert(80)

    print("Inorder traversal:")
    bst.inorder()  # Виводить елементи дерева у порядку зростання

    print("Search 20:", bst.search(20))  # Перевірка наявності елемента 20
    print("Search 90:", bst.search(90))  # Перевірка наявності елемента 90

    bst.delete(20)  # Видалення елемента 20
    print("Inorder traversal after deletion:")
    bst.inorder()  # Виведення дерева після видалення елемента 20
