# LLM Marketing Content Measurement and Optimization Architecture

## 1. Objective

Evolve the current LLM marketing measurement system from:

```text
Marketing Activity Counts
        ↓
Adstock
        ↓
Sentiment by Intent Type
        ↓
Engaged Visits
        ↓
Randomized Probability Matching (RPM)
        ↓
Marketing Activity Ranking
```

into a richer system that models the **actual information being introduced into the environment**:

```text
Actual Marketing Content
        ↓
LLM-Engineered Content Features
        ↓
LLM Response Measures by Intent
        ↓
Business Outcomes
        ↓
Hierarchical Bayesian Models
        ↓
Randomized Probability Matching
        ↓
Content Scoring / Simulation / Recommendations
```

The fundamental shift is from asking:

> Which marketing activity type performs best?

to:

> What characteristics of marketing content are most likely to change commercially important LLM responses?

Activity type can remain useful metadata, an organizational/reporting dimension, and potentially a pooling variable, but it no longer needs to represent the underlying mechanism.

---

# 2. Current Architecture

The current system contains two hierarchical Bayesian models.

## 2.1 Marketing Activity → Sentiment

There are:

* 24 marketing activities
* 3 strategic areas
* 17 prompt intent types
* weekly activity counts
* adstocked activity counts
* sentiment calculated from LLM responses

The hierarchy is:

```text
Global Marketing Effect
        ↓
3 Strategic Areas
        ↓
24 Marketing Activities
        ↓
Sentiment by Intent Type
```

The global model initially treats marketing activities as equivalent and estimates an overall effect.

Strategy-level effects are allowed to deviate from the global effect.

Individual marketing activities are then allowed to deviate from their parent strategy.

This hierarchical pooling is necessary because the longitudinal sample is small relative to the number of potential parameters.

---

## 2.2 Sentiment → Engaged Visits

The second model estimates the relationship between LLM sentiment and downstream business performance.

The hierarchy is approximately:

```text
Global LLM Sentiment
        ↓
17 Intent-Specific Sentiment Effects
        ↓
Engaged Visits
```

The global relationship is anchored using the longer-history top-20 prompt panel.

Intent-specific relationships are allowed to deviate from the global relationship.

---

# 3. Current RPM Decision System

Posterior coefficient means and credible intervals from the Bayesian models are passed into Randomized Probability Matching.

RPM first ranks the 17 intent types according to their relationship with engaged visits.

Marketing activities are then RPM-ranked separately within each intent according to their relationship with sentiment.

The final activity score is approximately:

$$
R_j=\sum_{g=1}^{17}w_g r_{jg}
$$

where:

* \(w_g\) = RPM importance of intent \(g\)
* \(r_{jg}\) = RPM score of marketing activity \(j\) within intent \(g\)

Conceptually:

```text
Importance of Intent
        ×
Ability of Activity to Move That Intent
        ↓
Final Marketing Activity Ranking
```

---

# 4. Data History

There is an important historical asymmetry.

### Top-20 prompts

Approximately:

$$
20 \times 30 = 600
$$

prompt-week observations across roughly 30 weeks.

### Remaining 2,680 prompts

Approximately:

$$
2680 \times 7 = 18,760
$$

prompt-week observations across roughly 7 weeks.

Both sets continue to be collected.

The larger prompt set has previously required variance matching so that its much greater cross-sectional sample size does not mechanically dominate the smaller but longer-history top-20 panel.

This unequal history must remain explicitly represented in future models.

---

# 5. Future Marketing Content Representation

Marketing activity counts should be replaced or augmented by features extracted from the **actual marketing content**.

A frozen/versioned LLM evaluator assesses each content object.

For content item \(c\):

$$
X_c =
[
x_{c1},x_{c2},...,x_{cK}
]
$$

Candidate features include:

* novelty / incremental information
* factual validity
* evidence strength
* authority / source credibility
* significance / materiality
* specificity
* factual support
* information gain
* freshness
* expected ability to materially change an LLM answer
* expected relevance to each intent
* expected onset of impact
* expected peak impact
* expected persistence
* expected decay
* corroboration with existing content
* complementarity with existing content
* redundancy with existing content
* contradiction with existing information
* source diversity
* competitive implications

The feature ontology should remain relatively compact.

Do **not** generate dozens of highly correlated semantic scores merely because an LLM can produce them.

---

# 6. Interaction Modeling

The important interactions are not primarily interactions between features within one content item.

The important question is whether **different pieces of marketing content reinforce one another**.

Examples include:

```text
Owned Content + Independent PR
        ↓
Greater Credibility / Corroboration

New Claim + Existing Supporting Evidence
        ↓
Greater LLM Answer Impact

Two Nearly Identical Content Pieces
        ↓
Redundancy / Saturation
```

With 24 activity types there are:

$$
{24 \choose 2}=276
$$

possible pairwise activity interactions.

Estimating 276 additional interaction parameters from 7–30 weeks of data is not credible.

Instead, the LLM feature-engineering layer should evaluate the interaction environment directly.

For example:

$$
Z_c =
[
Corroboration,
Complementarity,
Redundancy,
Contradiction,
SourceDiversity,
Reinforcement,
...
]
$$

The Bayesian model then determines whether these **generalized interaction properties** empirically predict changes in LLM responses.

Thus:

```text
LLM
    → infer semantic interaction structure

Bayesian Model
    → estimate whether those inferred structures actually matter
```

---

# 7. Temporal Effects

Generic adstock currently provides a manually specified temporal memory mechanism:

$$
X_t^{adstock}
=
X_t+\lambda X_{t-1}+\lambda^2X_{t-2}+\cdots
$$

Future content engineering can instead estimate timing from the actual content.

For each content item, the LLM can estimate:

* expected onset
* expected peak
* expected duration
* expected persistence
* expected decay profile

This defines a content-specific temporal kernel:

$$
K_c(\tau)
$$

The effective content state becomes:

$$
E_t
=
\sum_c K_c(t-t_c)X_c
$$

This avoids asking a very short time series to independently discover arbitrary temporal dynamics.

Eventually, empirical data can calibrate or correct the LLM-estimated kernels.

For example:

$$
K_c^{actual}(\tau)
=
K_c^{LLM}(\tau)
+
\Delta K_c(\tau)
$$

with strong shrinkage toward the LLM prior when longitudinal evidence is weak.

---

# 8. Future LLM Response Measurement

Sentiment should become **one response measure among several**, rather than the complete representation of LLM behavior.

For intent \(g\) at week \(t\):

$$
L_{g,t}
=
[
L_{1,g,t},
L_{2,g,t},
...,
L_{M,g,t}
]
$$

Potential autonomous response measures include:

* sentiment
* semantic position
* semantic movement
* semantic dispersion
* response consistency
* certainty
* specificity
* subjectivity
* other validated semantic dimensions

The measures should be:

1. autonomously computable;
2. applicable across the actual prompt population;
3. historically backfillable where possible;
4. reproducible;
5. empirically validated against business outcomes.

---

# 9. Repeated Prompt Generations

Going forward, multiple responses should be generated for the same prompt each week.

Instead of:

$$
R_{i,t}
$$

the system observes:

$$
R_{i,t,1},R_{i,t,2},...,R_{i,t,n}
$$

This allows estimation of the **response distribution**, rather than relying on one stochastic LLM generation.

For example, sentiment can have:

$$
E[S_{i,t}]
$$

and:

$$
Var(S_{i,t})
$$

Semantic embeddings can similarly produce:

* response centroid
* semantic dispersion
* week-over-week distributional shift
* response stability
* tail probabilities

Repeated generations improve **measurement precision**.

They do **not** create additional independent weeks of business-performance data.

---

# 10. Prompt and Intent Hierarchy

The 2,700 prompts and 17 intent types should serve different purposes.

```text
2,700 Individual Prompts
        ↓
17 Intent Types
        ↓
Business Outcome
```

The individual prompts provide:

* diagnostic resolution
* content relevance
* explanation
* opportunity discovery
* prompt-specific simulation

The 17 intent types provide:

* statistical pooling
* business-outcome estimation
* RPM authority
* interpretable aggregation

Therefore, the system does not need to choose between 2,700 prompts and 17 intents.

The two levels solve different problems.

---

# 11. Future Bayesian Architecture

The near-term production architecture should remain Bayesian rather than becoming an unconstrained deep neural network.

A reasonable description is:

```text
LLM Semantic Feature Engineering
        +
Hierarchical Multivariate Bayesian Modeling
        +
Randomized Probability Matching
```

Deep learning can eventually be introduced **inside the hierarchy** when sufficient longitudinal evidence exists.

It should not replace the hierarchy.

---

# 12. Upstream Hierarchical Model

The upstream model becomes:

$$
\text{Content Features}
\rightarrow
\text{LLM Response Measures by Intent}
$$

Hierarchical pooling should preserve the statistical discipline already required by the existing system.

Potential structure:

```text
Global Effect
        ↓
Feature / Feature Family
        ↓
Strategy, where useful
        ↓
Content / Activity
```

Activity type can remain a pooling or metadata dimension without being treated as the fundamental causal mechanism.

Multivariate Bayesian modeling can also account for covariance among response measures.

---

# 13. Downstream Hierarchical Model

Currently:

```text
Global Sentiment
        ↓
Sentiment by 17 Intents
        ↓
Engaged Visits
```

Future:

```text
Global LLM Response State
        ↓
Response Measure
        ↓
17 Intents Within Measure
        ↓
Engaged Visits
```

For response measure \(m\) and intent \(g\), the model produces posterior coefficients:

$$
\beta_{m,g}
$$

with hierarchical shrinkage toward the measure-level and global effects.

---

# 14. Expanded RPM Architecture

Adding multiple LLM response measures requires another RPM dimension.

For each response measure \(m\):

1. estimate its relationship with business performance;
2. RPM-rank the measures;
3. RPM-rank the 17 intents within that measure;
4. RPM-rank the content features within each measure × intent combination.

The final feature ranking becomes conceptually:

$$
R_k
=
\sum_m q_m
\sum_g w_{g|m}r_{k|g,m}
$$

where:

* \(q_m\) = RPM weight of response measure \(m\)
* \(w_{g|m}\) = RPM weight of intent \(g\) within measure \(m\)
* \(r_{k|g,m}\) = RPM score of content feature \(k\) for that intent and measure

The current system is a special case where:

$$
q_{sentiment}=1
$$

---

# 15. Role of Activity Type

Activity type does not necessarily need to disappear.

Instead, its role changes.

It can remain:

* organizational metadata
* reporting taxonomy
* budgeting dimension
* ownership dimension
* execution constraint
* possible hierarchical pooling variable

But the primary optimization target becomes:

> **What information should marketing introduce into the environment?**

rather than:

> Which internally defined activity category should marketing do more often?

The system can subsequently determine which activity types are best suited to produce the desired content characteristics.

Thus:

```text
What information characteristics matter?
        ↓
What content should be created?
        ↓
Which marketing vehicle can best produce/distribute it?
```

---

# 16. Product 1 — Content Scoring

Content teams should be able to submit a draft **before publication**.

The same frozen LLM evaluator used on historical content generates:

$$
X_{draft}
$$

The fitted Bayesian system predicts:

$$
X_{draft}
\rightarrow
\Delta L_{m,g}
$$

The system can then estimate the business relevance of those predicted changes.

The output should not merely be a score.

It should explain:

* predicted LLM impact
* affected response measures
* affected intents
* affected prompts
* expected timing
* uncertainty
* why the content is expected to matter
* what could be changed to improve the content

---

# 17. Product 2 — Content Simulation

The fitted system can simulate synthetic content profiles:

$$
X^{(1)},X^{(2)},...,X^{(N)}
$$

and propagate them through the model.

This allows questions such as:

> Which combinations of content characteristics are associated with the largest commercially relevant LLM changes?

For example, simulation may identify:

```text
High Novelty
+
High Significance
+
Strong Evidence
+
Independent Corroboration
+
High Specificity
```

as substantially more valuable than generic positive marketing language.

Simulation can also perform local perturbation:

$$
X_c+\delta_k
$$

to answer:

> If exactly one property of this draft could be improved, which one has the greatest expected value?

---

# 18. Product 3 — Prompt Impact Explorer

For proposed content \(C\) and prompt \(P_i\), estimate:

$$
P(
\text{Content materially changes answer to Prompt }i
)
$$

The system should rank the 2,700 prompts by expected impact.

It should also explain why.

For example:

> High expected impact because the prompt asks about career outcomes and the proposed content introduces new independently supported employment-outcome evidence directly relevant to the answer.

Or:

> Low expected impact because the content concerns the brand but introduces no information material to answering this particular prompt.

This provides prompt-level explanation while the 17 intents continue to provide the statistical pooling required for business modeling.

---

# 19. Product 4 — Content Opportunity Discovery

The system can reverse the problem.

Instead of asking:

> What will this draft affect?

ask:

> What information should we create next?

Candidate opportunities occur where:

* an intent has high business RPM importance;
* important prompts within that intent have weak, unstable, or undesirable LLM responses;
* existing published information does not adequately address those prompts;
* simulation suggests the response state is movable;
* feasible content characteristics exist that could address the gap.

This converts the system from a measurement tool into a **content strategy engine**.

---

# 20. Product 5 — Content Revision Guidance

Because proposed content is represented by interpretable features, poorly scoring content can be improved rather than simply rejected.

For example:

```text
Current Draft

Novelty: Low
Authority: High
Specificity: Moderate
Corroboration: Low
Intent Relevance: High
Redundancy: High
```

The system might recommend:

> Add independently verifiable evidence supporting the primary claim, introduce information not already present in existing content, and make the outcome more concrete.

The revised content can then be rescored before publication.

---

# 21. What Is Empirically Achievable Today

Different portions of the system have different levels of statistical authority.

## Tier 1 — Semantic Inference

The LLM evaluates:

* content properties
* timing
* interactions
* prompt relevance
* expected impact mechanisms

This requires relatively little historical time-series information.

## Tier 2 — LLM Response Validation

Observed responses determine whether the LLM-engineered content features actually predict changes in LLM behavior.

The 2,700-prompt panel and repeated generations make this increasingly powerful.

## Tier 3 — Business Association

Hierarchically pooled LLM response states are related to engaged visits and other business outcomes.

This is constrained by longitudinal sample size.

## Tier 4 — Causal Business Attribution

Claims such as:

> Publishing this article caused X additional engaged visits.

require substantially stronger identification through:

* experiments
* natural experiments
* deliberate content variation
* longer longitudinal histories
* or other credible causal designs

The system should not overstate Tier 3 evidence as Tier 4 evidence.

---

# 22. Primary Statistical Constraint

The major constraint is not the total number of prompt-response rows.

It is the number of independent temporal observations.

Currently:

```text
Top-20 Prompt History: ~30 weeks
Full 2,700-Prompt History: ~7 weeks
```

Therefore:

$$
n_{business}\approx30
$$

at best for the longer-history measures, while full-landscape measures currently have approximately:

$$
n_{full}\approx7
$$

weeks.

Repeated generations improve measurement precision but do not increase this temporal sample size.

---

# 23. Correlated Time-Series Data

Correlation does not make the system impossible.

However, the architecture must account for:

* autocorrelation
* correlated marketing features
* correlated LLM response measures
* common weekly shocks
* unequal measurement precision
* unequal historical coverage
* potentially correlated business outcomes

The greatest identification problem occurs when predictors do not vary independently.

For example:

$$
Novelty_t \approx Significance_t
$$

across nearly every historical observation.

In that case, separating the effects of novelty and significance may be impossible even with sophisticated modeling.

Therefore, the LLM feature ontology should favor **distinct, interpretable, minimally redundant dimensions**.

---

# 24. Unequal Prompt Histories

The top-20 and broader 2,700-prompt histories should not be treated as though they contain equivalent information.

The future model should ideally carry measurement uncertainty explicitly:

$$
Z_{g,t}^{observed}
\sim
N(
Z_{g,t}^{true},
\Sigma_{g,t}
)
$$

Different prompt groups and periods can therefore have different uncertainty.

More observations should result in **greater measurement precision**, not automatically greater business importance.

This is the principled extension of the current variance-matching approach.

---

# 25. Main Cautions

## Small Longitudinal Sample

Seven weeks of full coverage cannot support hundreds of freely estimated parameters.

Aggressive hierarchical shrinkage remains essential.

## Feature Redundancy

Do not allow the LLM to create dozens of slightly different semantic measurements.

Inspect covariance and collapse redundant features.

## LLM Feature Drift

The content evaluator must be versioned and
