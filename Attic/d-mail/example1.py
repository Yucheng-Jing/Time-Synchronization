# author: Pedro Garcia Lopez, PhD
from chord import *


n1 = Node(1)
n2 = Node(8)
n3 = Node(14)
n4 = Node(21)
n5 = Node(32)
n6 = Node(38)
n7 = Node(42)
n8 = Node(48)
n9 = Node(51)
n10 = Node(56)

n1.join(n1)
n2.join(n1)
n3.join(n1)
n4.join(n1)
n5.join(n1)
n6.join(n1)
n7.join(n1)
n8.join(n1)
n9.join(n1)
n10.join(n1)

showFinger(n1)
showFinger(n2)
showFinger(n3)
printNodes(n1)

#toPajek(n1,'michord.net')
#toPajek2(n1,'michord2.net')
#print 'finish !!!'
