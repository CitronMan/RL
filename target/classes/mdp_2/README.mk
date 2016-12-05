马尔科夫过程：
A state signal that succeeds in retaining all relevant　information is said to be Markov, or to have the Markov property 
转移概率
reward


1.先随机初始化一个策略 π0，计算这个策略下每个状态的价值 vv0
2.根据这些状态价值,遍历所有actions,找到最优actions（）,更新新策略 π1；
；根据这些状态价值得到新策略 π1；计算新策略下每个状态的价值 vv1 ... 直到收敛。
计算一个策略下每个状态的价值，被称为策略评估 (Policy Evaluation)；根据状态价值得到新策略，被称为策略改进 (Policy Improvement)。


