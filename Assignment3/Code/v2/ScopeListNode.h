#include <iostream>
#include <string>

using namespace std;


template <class T>
class MyNode {
	private:
		T* data;
	public:
		MyNode* next;
		MyNode* prev;
		MyNode(T* data);
		T* getData();
		~MyNode();
};

template <class T>
MyNode<T>::MyNode(T* data) {
	this->data = data;
}

template <class T>
T* MyNode<T>::getData() {
	return this->data;
}

template <class T>
MyNode<T>::~MyNode() { }




template <class T>
class MyList {
	private:
		T* head;
		T* tail;
		int list_size;
	public:
		MyList(T* node);
		T* getHead();
		T* getTail();
		int size(bool update=false);
		void addNodeAsTail(T* new_node);
		void addNodeAsHead(T* new_node);
		void push(T* new_node);
		T* pop();
		void enqueue(T* new_node);
		T* dequeue();
		T* get(int index);
		void printList();
		~MyList();
};

template <class T>
MyList<T>::MyList(T* node) {
	this->head = node;    
	this->tail = this->head;
    node->next = this->tail;
    node->prev = this->head;
	this->list_size = 1;
}

template <class T>
T* MyList<T>::getHead() {
	return this->head;
}

template <class T>
T* MyList<T>::getTail() {
	return this->tail;
}

template <class T>
int MyList<T>::size(bool update) {
	if (!update) {
		return this->list_size;
	}
	int size = 0;
	T* temp = this->head;
	while (temp) {
		size++;
		temp = temp->next;
	}
	this->list_size = size;
	return this->list_size;
}

template <class T>
void MyList<T>::addNodeAsTail(T* new_node) {
	new_node->next = NULL;
	new_node->prev = NULL;
	
	if (this->head == NULL) {
		this->head = new_node;
		this->tail = this->head;
		this->list_size = this->list_size + 1;
	} else {
		this->tail->next = new_node;
		new_node->prev = this->tail;
		new_node->next = this->head;
		this->head->prev = new_node;
		this->tail = new_node;
		this->list_size = this->list_size + 1;		
	}
}

template <class T>
void MyList<T>::addNodeAsHead(T* new_node) {
	new_node->next = NULL;
	new_node->prev = NULL;
	
	if (this->head == NULL) {
		this->head = new_node;
		this->tail = this->head;
		this->list_size = this->list_size + 1;
	} else {
		this->head->prev = new_node;
		new_node->next = this->head;
		this->head = new_node;
		this->list_size = this->list_size + 1;
	}
}

template <class T>
void MyList<T>::push(T* new_node) {
	this->addNodeAsHead(new_node);
}

template <class T>
T* MyList<T>::pop() {
	T* temp = this->head;
	this->head = this->head->next;
	this->head->prev = NULL;
	this->list_size = this->list_size - 1;
	return temp;
}


template <class T>
void MyList<T>::enqueue(T* new_node) {
	this->addNodeAsTail(new_node);
}

template <class T>
T* MyList<T>::dequeue() {  // 			A B C D  tmp=D tail=C A-prev = C
    T* temp = this->tail;
    this->tail = temp->prev;
    temp->prev->next = this->head;
	temp->next->prev = this->tail;
	this->list_size = this->list_size - 1;
	return temp;
}

template <class T>
T* MyList<T>::get(int index) {
	if (index == 0) {
		return this->head;
	} else if (index == this->list_size - 1) {
		return this->tail;
	} else if (index < 0 || index >= this->list_size) {
		return NULL;
	}
	if (index < this->list_size / 2) {
		T* temp = this->head;
		int i = 0;
		while (temp) {
			if (i == index) { return temp; }
			i++;
			temp = temp->next;
		}
	} else {
		T* temp = this->tail;
		int i = this->list_size - 1;
		while (temp) {
			if (i == index) { return temp; }
			i--;
			temp = temp->prev;
		}
	}
	return NULL;
}

template <class T>
void MyList<T>::printList() {
	cout << "HEAD: ";
	T* temp = this->head;
	while(temp) {
		cout << temp->toString() << " -> ";
		temp = temp->next;
	}
	cout << "\b\b\b\b :TAIL" << endl;
}

template <class T>
MyList<T>::~MyList() {
	while (this->head) {
		T* next = this->head->next;
		delete this->head;
		this->list_size = this->list_size - 1;
		this->head = next;
	}
}
