# primary-crowding-numericalproof

Numerical characterization of action‐set structures in a two‑stage primary–general election model of candidate behavior.

## Overview
This repository computes and visualizes how primary crowding changes candidates’ incentives to take a nomination‑focused action that boosts primary performance but reduces general‑election viability. The numerical exercise supports the paper’s theoretical results by mapping the **action region**—the set of candidate types who choose the action—across a wide parameter space and documenting comparative statics.

## Theoretical model (brief)
- **Candidates and types.** There are $N$ candidates with valences $v_i \in \mathbb{R}$, drawn i.i.d. from $F$ with positive density. The realized profile $\mathbf v$ is observed by all candidates.
- **Action.** Each candidate chooses $a_i \in \{0,1\}$. Choosing $a_i=1$ improves primary standing but imposes a general‑election penalty $\delta>0$.
- **Primary stage.** Primary index: $q_i^P = v_i + a_i + \varepsilon_i$ with i.i.d. EV1 shocks. Nomination probability is multinomial logit:
  $$ 
  \Pr(w=i \mid \mathbf v, \mathbf a) = \frac{e^{v_i+a_i}}{\sum_j e^{v_j+a_j}}.
  $$
- **General election.** If nominee $i$ wins the primary, general‑election index is $q_i^{GE}=v_i-\delta a_i+\eta_i$ with $\eta_i\sim N(0,1)$ and opponent normalized to 0. Win probability is $\Phi(v_i-\delta a_i)$.
- **Payoff.** Expected payoff equals nomination probability times general‑election win probability.

### Reduced‑form incentive comparison
Holding opponents fixed, summarize opponent strength by
$$ 
S \equiv \sum_{j\neq i} e^{v_j+a_j}.
$$
Define
$$
p_a(v;S)=\frac{e^{v+a}}{e^{v+a}+S},\quad
G(v;S)=\frac{p_1(v;S)}{p_0(v;S)},\quad
C(v;\delta)=\frac{\Phi(v)}{\Phi(v-\delta)}.
$$
The action is optimal iff
$$
\Delta(v;S)=p_1(v;S)\Phi(v-\delta)-p_0(v;S)\Phi(v) \ge 0
\quad\Longleftrightarrow\quad G(v;S)\ge C(v;\delta).
$$
The **action region** is $A(S)=\{v: G(v;S)\ge C(v;\delta)\}$.

## Purpose of the numerical exercise
The analysis documents how $A(S)$ changes with **primary crowding** ($S$) and **general‑election penalty** ($\delta$), and classifies the shape of $A(S)$ over economically relevant $v$.

Key questions:
- Does $A(S)$ form a single upper‑tail cutoff or more complex intervals?
- Is there a penalty threshold $\delta^*$ that governs whether screening occurs?
- How do cutoffs move with $S$ and $\delta$?

## Methodology (Appendix B)
The numerical exercise evaluates
$$ 
H(v)=G(v;S)-C(v;\delta)
$$
on a dense grid and classifies the action set by counting zero‑crossings.

**Parameter grid**
- $\delta \in [0.01,0.50]$ (step 0.01; 50 values)
- $S \in [2,200]$ (step 1; 199 values)
- Total parameter pairs: **9,950**

**Type grid**
- $v \in [-10,10]$ with step 0.1 (economically relevant range).
- Bounds justified by tail guarantees and coverage of $>99.999\%$ of candidates under $N(0,4)$, with Chebyshev support for broader distributions.

**Computation**
- Compute $G(v;S)=1+\frac{(e-1)S}{e^{v+1}+S}$ and $C(v;\delta)=\frac{\Phi(v)}{\Phi(v-\delta)}$.
- Evaluate $H(v)$ on the grid and detect sign changes.
- Zero‑crossings are located by linear interpolation between adjacent grid points (precision ≈ ±0.05).

## Outputs and main numerical findings
- **Two regimes observed** within $v\in[-10,10]$:
  1. **All‑action regime** for $\delta<\delta^*\approx 0.10$: $H(v)>0$ throughout the grid (true lower threshold lies left of −10).
  2. **Screening regime** for $\delta\ge \delta^*\approx 0.10$: exactly one zero‑crossing, yielding an upper‑tail action set $[v^*(S),\infty)$.  
- **No interior‑interval or multi‑crossing structures** were observed in the tested range.
- **Regime boundary is S‑invariant**: $\delta^*\approx 0.10$ for all tested $S$ (std. dev. < 0.0001).
- **Comparative statics (screening regime):**
$$ 
v^*(S,\delta)\approx 18.5\,\delta-0.003\,S\quad (R^2=0.98).
$$
  Penalty effects dominate crowding effects in magnitude, but crowding still shifts the participation margin.

## How proofs and comparative statics are computed
- **Analytical proofs** establish monotonicity and tail behavior of $G(v;S)$ and $C(v;\delta)$, yielding the left‑tail guarantee and comparative statics for the action region.
- **Numerical verification** (Appendix B) implements grid‑based evaluation of $H(v)$ across 9,950 parameter pairs, classifies action‑set structure by zero‑crossings, and estimates $v^*(S,\delta)$ by interpolation and regression.
- **Comparative statics** are computed by regressing extracted cutoffs on $(\delta,S)$ in the screening regime and by verifying monotonic inclusion of action sets across $S$ and $\delta$.

## Citation
If you use this code or results, please cite: 
Parslow, Katherine (2026). ``Crowded Primaries and Weakened Nominees: How Competition Distorts Candidate Behavior." Working Paper.
