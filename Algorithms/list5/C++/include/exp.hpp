#ifndef EXP_HPP 
#define EXP_HPP

void runHeapExperiment(int n, int trials);
void runGraphExperiment(int step, int limit, int trials, bool useKruskal);
void runSchedulingExperiment(int step, int limit, int trials, int rootChoicesByN);

#endif // EXP_HPP