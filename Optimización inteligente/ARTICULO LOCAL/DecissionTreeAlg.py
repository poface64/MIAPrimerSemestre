import pandas as pd
import numpy as np
from typing import Dict, List
import random
from queue import Queue
import copy
import math
import time
import sys
import psutil
import os

class DecisionTree:
    class ClassVal:
        def __init__(self, name, values):
            self.name = values
            self.values=name

    class Attribute:
        name:str
        number:int

        def __init__(self, name, number, values):
            self.name = name
            self.number=number
            self.values=values

    class Node:
        root:bool
        nodeAttr=None  #This is an attribute
        connections=[] #list of dict[string,node]
        def __init__(self, root, attr,parentVal,classAttr,parentNode,index,level):
            self.root = root
            self.nodeAttr=attr
            self.classAttr=classAttr #If its a normal attr or if it's a class attribute
            if(not classAttr):
                self.unusedValues=self.nodeAttr.values.copy()
            else:
                self.unusedValues=None
            self.parentVal=parentVal
            self.connections=[]
            self.parentNode=parentNode
            self.index=index
            self.level=level
            
        
        def createConnections(self,unusedAttr,classAttributes,index,level):
            newConnections=[]
            for value in self.nodeAttr.values:
                attrType,attr=self.getRandomAttrOrClass(unusedAttr,classAttributes)
                nodeConnection=DecisionTree.Node(False,attr,value,attrType,self,index,level+1)
                index+=1
                nodeConnection.connections=[]
                self.connections.append(nodeConnection)
                newConnections.append(nodeConnection)
                if(not attrType):
                    unusedAttr.remove(attr)
            return unusedAttr,newConnections,index

        def getRandomAttrOrClass(self,unusedAttr,classAttributes):
            if(len(unusedAttr)>0):
                classAttr = random.choices(['class', 'attr'], weights=[25, 75], k=1)[0]
            else:
                classAttr='class'
            if(classAttr=='class'):
                return True,random.choice(classAttributes)
            else:
                return False,random.choice(unusedAttr)
            
        def changeConnection(self,oldNode,NewNode):
            self.connections.remove(oldNode)
            self.connections.append(NewNode)



    attributes: list[Attribute]
    unusedAttributes: list[Attribute]
    nodes: list[Node]


    def __init__(self, data):
        # todo 
        if isinstance(data, DecisionTree):
            self.data=data.data
            self.attributes = self.getAttributes(self.data)
            self.unusedAttributes=[]
            self.currentIndex=data.currentIndex
            self.classes=self.getClasses(self.data)
            self.nodes=copy.deepcopy(data.nodes)  
            self.classNodes=[]
            self.attrNodes=[]
            self.maxLevel=data.maxLevel
            for nodei in self.nodes:
                if(nodei.classAttr):
                    self.classNodes.append(nodei)
                else:
                    self.attrNodes.append(nodei)
            self.rootNode=copy.deepcopy(data.rootNode)
        #if isinstance(data, str):
        else :
            # todo 
            self.currentIndex=0
            self.data=data
            self.attributes = self.getAttributes(data)
            self.unusedAttributes=[]
            self.classes=self.getClasses(data)
            self.nodes=[]   
            self.classNodes=[]
            self.attrNodes=[]

    def getAttributes(self,data):
        attributes=[]
        index=0
        for column in data.columns[:-1]:
            unique_values = data[column].unique()
            attribute=DecisionTree.Attribute(str(column),index,unique_values)
            attributes.append(attribute)
            #print(f"Column {column} unique values:", unique_values)
            index+=1
        return attributes
    
    def getClasses(self,data):
        classVals=[]
        for column in data.columns[-1:]:
            unique_values = data[column].unique()
            for val in unique_values:
                classVal=DecisionTree.ClassVal(str(column),val)
                classVals.append(classVal)
            return classVals
        
    def createRandomTree(self):
        self.rootNode=[]
        self.nodes=[]
        self.unusedAttributes=[]
        self.unusedAttributes=self.attributes.copy()
        attr=random.choice(self.unusedAttributes)
        self.unusedAttributes.remove(attr)
        leftConnections=Queue()
        node=self.Node(True,attr,None,False,None,self.currentIndex,0)
        self.currentIndex+=1
        self.rootNode=node
        self.nodes.append(node)
        self.unusedAttributes,newConnections,self.currentIndex=node.createConnections(self.unusedAttributes,self.classes,self.currentIndex,0)
        
        #print(newConnections)
        for newConn in newConnections:
            self.nodes.append(newConn)
            if(not newConn.classAttr):
                leftConnections.put(newConn)      
        #print(leftConnections)          
        while True:
            if leftConnections.empty():
                break
            currentNode=leftConnections.get()
            self.unusedAttributes,newConnections,self.currentIndex=currentNode.createConnections(self.unusedAttributes,self.classes,self.currentIndex,currentNode.level)
            #print(newConnections)
            for newConn in newConnections:
                self.nodes.append(newConn)
                if(not newConn.classAttr):
                    leftConnections.put(newConn)  
            #print(leftConnections)
        self.classNodes=[]
        self.attrNodes=[]
        for nodei in self.nodes:
            if(nodei.classAttr):
                self.classNodes.append(nodei)
            else:
                self.attrNodes.append(nodei)
        self.recreateTreeLevels()

    def printTree(self):
        print(self.nodes[0].nodeAttr.name)
        for connection in self.nodes[0].connections:
            self.printConnection(1,connection)

    def printConnection(self,level,node):
        levelRep=""
        for i in range(level):
            levelRep=levelRep+"\t"
        if(node.classAttr):
            print(levelRep+str(node.parentVal)+" - "+node.nodeAttr.name)            
        else:
            print(levelRep+str(node.parentVal)+" "+node.nodeAttr.name)            
        for connection in node.connections:
            self.printConnection(level+1,connection)
        
    def GetAccuracy(self,data):
        corrects=0        
        #data2 = data.iloc[:400]
        for row in data.itertuples():
            evaluation=self.DoInference(row,self.rootNode)
            #print(evaluation)            
            #print(row[-1])
            if(evaluation==row[-1]):
                #print("correct")
                corrects+=1
            else:
                #print("incorrect")
                pass
        self.accuracy=corrects/data.shape[0]
        return self.accuracy


    def DoInference(self,dataRow,node):
        if(node.classAttr):
            return node.nodeAttr.name
        else:
            attrVal = getattr(dataRow, node.nodeAttr.name)
            #attrVal=dataRow.loc[node.nodeAttr.name]
            for connection in node.connections:
                #print(connection.parentVal)
                if(connection.parentVal==attrVal):
                    return self.DoInference(dataRow,connection)

    def mutarArbol(self):
        nodeSource, nodeTarget,isAttrOrClass=self.GetmutationTargets()
        if(nodeSource==None):
            self.createRandomTree()
        elif(isAttrOrClass):
            nodeSource.parentNode.changeConnection(nodeSource,nodeTarget)
            nodeTarget.parentNode.changeConnection(nodeTarget,nodeSource)
            parentSource=nodeSource.parentNode
            nodeSource.parentNode=nodeTarget.parentNode
            nodeTarget.parentNode=parentSource

            valSource=nodeSource.parentVal
            nodeSource.parentVal=nodeTarget.parentVal
            nodeTarget.parentVal=valSource
        else:
            nodeSource.parentNode.changeConnection(nodeSource,nodeTarget)
            nodeTarget.parentNode.changeConnection(nodeTarget,nodeSource)
            parentSource=nodeSource.parentNode
            nodeSource.parentNode=nodeTarget.parentNode
            nodeTarget.parentNode=parentSource

            valSource=nodeSource.parentVal
            nodeSource.parentVal=nodeTarget.parentVal
            nodeTarget.parentVal=valSource
        self.recreateTreeLevels()

    def getNodeForCombination(self,level=0,getByLevel=False):
        nodeSource=None
        nodeInLevel=[]
        if(getByLevel):
            for i in self.nodes:
                if(i.level==level):
                    nodeInLevel.append(i)
            nodeSource=random.choice(nodeInLevel)
        else:
            nodeSource=random.choice(self.nodes)
        #nodeSource=self.nodes[selection]
        while True:
            if(nodeSource.root):
                if(getByLevel):
                    nodeSource=random.choice(nodeInLevel)
                else:
                    nodeSource=random.choice(self.nodes)
            else:
                break        
        nodeParent=nodeSource.parentNode
        nodeValue=nodeSource.parentVal
        nodeParent.connections.remove(nodeSource)
        self.changenodes=[]
        self.getChildNodes(nodeSource)
        for nodess in self.changenodes:
            self.nodes.remove(nodess)
            if(nodess.classAttr):
                self.classNodes.remove(nodess)
        self.nodes.remove(nodeSource)
        return nodeSource, nodeParent,nodeValue, self.changenodes

    def addBranch(self,newNode,nodeParent,nodeval,allnodes):        
        self.nodes.append(newNode)
        if(newNode.classAttr):
                self.classNodes.append(newNode)
        nodeParent.connections.append(newNode)
        newNode.parentNode=nodeParent
        newNode.parentVal=nodeval
        for i in allnodes:
            self.nodes.append(i)
            if(i.classAttr):
                self.classNodes.append(i)
        self.recreateTreeLevels()

    def recreateTreeLevels(self):
        self.setTreeLevel(self.rootNode,0)
        self.maxLevel=0
        for i in self.nodes:
            if(i.level>self.maxLevel):
                self.maxLevel=i.level

    def setTreeLevel(self,node,level):
        node.level=level
        for i in node.connections:
            self.setTreeLevel(i,level+1)


    def getChildNodes(self,sourceNode):        
        for nodes in sourceNode.connections:
            self.changenodes.append(nodes)
            self.getChildNodes(nodes)

    def GetmutationTargets(self):
        classAttr = random.choices(['class', 'attr', 'recreateTree'], weights=[50, 50, 20], k=1)[0]
        if(classAttr=='class'):
        #if(True):
            index=0
            while True:
                nodeSource=random.choice(self.classNodes)
                nodeTarget=random.choice(self.classNodes)
                if(nodeSource!=nodeTarget and nodeSource.nodeAttr.name!=nodeTarget.nodeAttr.name):
                    break
                index+=1
                if(index>20):
                    return None,None,None
            return nodeSource, nodeTarget,True
        elif(classAttr=='attr'):
            while True:
                nodeSource=random.choice(self.nodes)
                nodeTarget=random.choice(self.nodes)
                if(nodeSource!=nodeTarget and not self.isBranchRelated(nodeSource,nodeTarget)):
                    break
            return nodeSource, nodeTarget,True
        else:
            return None,None,None
        
    def GetmutationTargets2(self):
        index=0
        while True:
            nodeSource=random.choice(self.classNodes).parentNode
            nodeTarget=random.choice(self.classNodes).parentNode
            if(nodeSource!=nodeTarget and nodeSource.nodeAttr.name!=nodeTarget.nodeAttr.name):
                break
            index+=1
            if(index>20):
                return None,None,None
        return nodeSource, nodeTarget,True
        

    def getNodesByLevel(self):
        levels=0
        for nodei in self.nodes:
            if(nodei.level>levels):
                levels=nodei.level


    def isBranchRelated(self,sourceNode,targetNode):
        related=False
        if(sourceNode.root or targetNode.root):
            return True
        parentNode=sourceNode.parentNode
        #Check if target node is parent of sourcenode
        while True:
            if(parentNode==targetNode):
                related=True
                break
            else:
                if(parentNode.root):
                    break
                parentNode=parentNode.parentNode

        parentNode=targetNode.parentNode
        while True:
            if(parentNode==sourceNode):
                related=True
                break
            else:
                if(parentNode.root):
                    break
                parentNode=parentNode.parentNode
                
        return related


    def getRandomAttrOrClass(self,unusedAttr,classAttributes):
        if(len(unusedAttr)>0):
            classAttr = random.choices(['class', 'attr'], weights=[10, 90], k=1)[0]
        else:
            classAttr='class'
        if(classAttr=='class'):
            return True,random.choice(classAttributes)
        else:
            return False,random.choice(unusedAttr)
    

def simulatedAnnealing(dataName):
    # Basic reading
    data = pd.read_csv(dataName)
    finish=False
    trainPercentage=0.8
    testData = data.sample(frac=1-trainPercentage, random_state=42)    
    trainData = data.drop(testData.index)
    raminingTrainData=trainData
    testDataPoints=len(testData)
    treesizes=[]
    leafsizes=[]
    accuracies=[]
    avarageTreeSize=0
    avarageLeafSize=0
    avarageTreeLevels=0
    avarageAccuracy=0
    finalTree=None


    initTime=time.time()
    for n in range(5):
        decisionTree=DecisionTree(data)
        decisionTree.createRandomTree()
        

        #print("Viejo Arbol")
        #decisionTree.printTree()
        #accuracy=decisionTree.GetAccuracy(data)
        #print("accuracy: ")
        #print(accuracy)

        accuracy=decisionTree.GetAccuracy(trainData)
        #print(accuracy)
        Temp=1000
        originalTemp=Temp
        overAllBest=None
        overAllBestAcc=accuracy
        for i in range(1000):
            if(Temp<700):
                ja=2
            if(Temp<500):
                    ja=2        
            if(Temp<300):
                    ja=2 
            if(Temp<100):
                ja=2               
            newDecisionTree=DecisionTree(decisionTree)
            #for i in range(random.randint(1, 10)):
            muations=math.ceil(Temp/(originalTemp/10))
            if(muations>7):
                muations=10
            if(muations<1):
                muations=1
            muations=random.randint(1,muations)
            for i in range(muations):
                newDecisionTree.mutarArbol()
                #size = sys.getsizeof(newDecisionTree)
                #print(f"Basic size: {size} bytes")
            newAccuracy=newDecisionTree.GetAccuracy(trainData)
            if(newAccuracy>accuracy):
                if(newAccuracy>overAllBestAcc):
                    overAllBest=newDecisionTree
                    overAllBestAcc=newAccuracy
                decisionTree=newDecisionTree        
                #print("Nuevo Arbol")
                #print("old accuracy: " +str(accuracy))
                #print("new accuracy: " +str(newAccuracy))
                accuracy=newAccuracy
                #Temp-=1
                #print("new best tree")
                #print(accuracy)
                if(accuracy>=1):
                    break
            elif(random.choices([True, False], weights=[Temp, originalTemp*10])[0]):
                accDiff=accuracy-newAccuracy
                accDiff*=100
                if(accDiff>15 or accDiff==0):
                    continue
                accPerc=(1+((20-accDiff)/100))*(Temp/8)
                if(random.choices([True, False], weights=[accPerc, originalTemp])[0]):
                    decisionTree=newDecisionTree        
                    #print("Nuevo Arbol")
                    #print("old accuracy: " +str(accuracy))
                    #print("new accuracy: " +str(newAccuracy))
                    accuracy=newAccuracy
                    Temp-=5
                    #print("new worst tree")
                    #print(accuracy)
                    if(accuracy>=1):
                        break
                    if(Temp<=1):
                        break
            

            #newDecisionTree.mutarArbol()
            #newDecisionTree.mutarArbol()
            #decisionTree.printTree()    
        #print("Best final: ")
        accuracy=decisionTree.GetAccuracy(testData)
        print("Accuracy iteration "+str(n) + " "+str(accuracy))
        print("Tree size: " +str(len(decisionTree.nodes)))
        print("Tree levels: " +str(decisionTree.maxLevel))
        print("Tree leafs: " +str(len(decisionTree.classNodes)))
        treesizes.append(len(decisionTree.nodes))
        leafsizes.append(len(decisionTree.classNodes))
        accuracies.append(accuracy)
        avarageTreeSize+=len(decisionTree.nodes)
        avarageLeafSize+=len(decisionTree.classNodes)
        avarageTreeLevels+=decisionTree.maxLevel
        avarageAccuracy+=accuracy
        finalTree=decisionTree
        #decisionTree.printTree()

        #print("Best overall: ")
        #overAllBestAcc=overAllBest.GetAccuracy(testData)
        #print("Final accuracy: " +str(overAllBestAcc))
        #overAllBest.printTree()
        if(finish):
            break
        if(len(raminingTrainData)<testDataPoints):
            testData = raminingTrainData
            finish=True
            #print("Memory Usage")
            #print(get_memory_usage())
        else:
            testData = raminingTrainData.sample(frac=testDataPoints/len(raminingTrainData), random_state=42)    
        raminingTrainData=raminingTrainData.drop(testData.index)
        trainData = data.drop(testData.index)

    print("Avarage accuracy: " +str(avarageAccuracy/5))
    print("sd accuracy: " +str(np.std(accuracies, ddof=1)))
    totaltime=time.time()-initTime
    print("Total time :")
    print(totaltime)
    print("Avarage tree size: " +str(avarageTreeSize/5))
    print("sd tree size: " +str(np.std(treesizes, ddof=1)))
    print("Avarage tree leafs: " +str(avarageLeafSize/5))
    print("sd leafs: " +str(np.std(leafsizes, ddof=1)))
    print("Avarage tree levels: " +str(avarageTreeLevels/5))
    finalTree.printTree()
    #print("Memory Usage")
    #print(get_memory_usage())



def createChildTree(tree1,tree2,doByLevel=False):
    newtree1=DecisionTree(tree1)
    newtree2=DecisionTree(tree2)
    #print("trees before combining")
    #tree1.printTree()
    #tree2.printTree()
    if(doByLevel):
        #,level=0,getByLevel=False
        level1=newtree1.maxLevel
        level2=newtree2.maxLevel
        level=0
        if(level1>level2):
            level=level2
        else:
            level=level1
        if(level>1):
            level=random.randrange(1,level)
        nodeSource1, nodeParent1,nodeValue1, changenodes1=newtree1.getNodeForCombination(level=level,getByLevel=True)
        nodeSource2, nodeParent2,nodeValue2, changenodes2=newtree2.getNodeForCombination(level=level,getByLevel=True)
    else:
        nodeSource1, nodeParent1,nodeValue1, changenodes1=newtree1.getNodeForCombination()
        nodeSource2, nodeParent2,nodeValue2, changenodes2=newtree2.getNodeForCombination()
    #print("Change to tree1")
    #print(str(nodeSource1.parentVal)+" - "+str(nodeSource1.nodeAttr.name))
    #print("Change to tree2")
    #print(str(nodeSource2.parentVal)+" - "+str(nodeSource2.nodeAttr.name))
    
    newtree1.addBranch(nodeSource2, nodeParent1,nodeValue1, changenodes2)
    newtree2.addBranch(nodeSource1, nodeParent2,nodeValue2, changenodes1)
    #print("trees after combining")
    #tree1.printTree()
    #tree2.printTree()
    return newtree1,newtree2

def genetic(sizePop,iterations,dataName,doByLevel=False):
    pass
    # Basic reading
    data = pd.read_csv(dataName)

    trainPercentage=0.7
    trainData = data.sample(frac=trainPercentage, random_state=42)
    testData = data.drop(trainData.index)

    trees=[]
    for i in range(sizePop):
        newDecisionTree=DecisionTree(data)
        newDecisionTree.createRandomTree()
        newDecisionTree.GetAccuracy(trainData)
        trees.append(newDecisionTree)
    
    trees = sorted(trees, key=lambda x: x.accuracy, reverse=True)
    currentIteration=0
    while True:
        print("iteracion: "+str(currentIteration))
        newTrees=[]
        for i in range(int((sizePop/2)-2)):
            parent1 = random.choices(trees, 
                              weights=[tree.accuracy for tree in trees],
                              k=1)[0]                              
            parent2 = random.choices(trees, 
                              weights=[tree.accuracy for tree in trees],
                              k=1)[0]
            while True:
                if(parent1!=parent2):
                    break
                else:
                    parent2 = random.choices(trees, 
                              weights=[tree.accuracy for tree in trees],
                              k=1)[0]
            newtree1,newtree2=createChildTree(parent1,parent2,doByLevel)
            newTrees.append(newtree1)
            newTrees.append(newtree2)
        #keep best two trees
        newTrees.append(trees[0])
        newTrees.append(trees[1])
        for i in range(sizePop-len(newTrees)-1):
            #Complete size pop with new randoms
            newDecisionTree=DecisionTree(data)
            newDecisionTree.createRandomTree()
            newTrees.append(newDecisionTree)

        trees=newTrees

        for i in trees:
            if(random.choices([True, False], weights=[9,1])[0]):
                pass
                #print("number of nodes before")
                #print(len(i.nodes))
                #i.mutarArbol()
                #print("number of nodes After")
                #print(len(i.nodes))

        for i in trees:
            i.GetAccuracy(trainData)

        trees = sorted(trees, key=lambda x: x.accuracy, reverse=True)

        currentIteration+=1
        if(currentIteration>=iterations):
            break

    print("Best1: ")
    accuracy=trees[0].GetAccuracy(testData)
    print("Final accuracy: " +str(accuracy))
    trees[0].printTree()

    print("Best2: ")
    accuracy=trees[1].GetAccuracy(testData)
    print("Final accuracy: " +str(accuracy))
    trees[1].printTree()


def get_memory_usage():
    process = psutil.Process(os.getpid())
    memory_info = process.memory_info()
    
    # Memory in MB
    memory_mb = memory_info.rss / 1024 / 1024  # RSS: Resident Set Size
    return f"Memory usage: {memory_mb:.2f} MB"

random.seed(43)
print("iris")
dataname='iriscaim.csv'
#genetic(50,100,dataname,False)
simulatedAnnealing(dataname)