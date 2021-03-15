## Research Questions

This study investigated product management practices in general with the ISPMA (2020) reference model and then roadmapping in particular with the DEEP v1.1 maturity model (Münch, Trieflinger, and Lang, 2019). It aims at understanding how SPM practices described by in the literature fit together with Software Product Manager (SPM) practices used in real life.This study addresses following research questions:

RQ1 How mature are roadmap processes used by SPMs?

RQ1.1 Are there factors that predict the maturity level? 

RQ1.2 What recommendations can be made to improve material for SPM?

And using the theoretical DEEP framework as a lens to compare the practices and industry bodies of knowledge. From the background literature review in forming these questions there are two soft hypothesis to help frame up the research approach:

1. Maturity will have some kind of link to industry, org size, and country the software product manager is based in.
2. The theories supported, in the different professional organisations and their Product Management Bodies of Knowledge (ProdBOK), are drawn from clusters of researchers particular to each organisation. i.e. there is fragmentation in transmission of theory to practice

This is assuming a linear, or successionist, explanation of causality (Horrocks and Fowles, 2012, section 2.3) that certain behaviours and processes lead to related outcomes, although the context that the actors are operating in - that is to say the Software Product Managers - may also have an influence on the outcome.

## Study Design

The factors for consideration in the research questions falls into two broad topics, the respondent and the organisation where they work:

+ Respondent, e.g.
    + Job title
    + Membership of professional bodies
    + Which areas of the SPM reference model they have responsibility for
    + Where they look for information on professional development
    + Do they feel happy with the process and their responsible areas
+ Respondent's organisation where they perform the role, e.g.
    + Location
    + Industrial sector
    + Size of org and product team
    + Update frequencies of releases and product launches
    + Maturity of roadmapping process

The three forms of data collection will be:
+ Questionnaire around SPM practices, this will go to a wide distribution and expect in the region of 100-1000 responses based on similar survey's results. The structure is in Appendix B (primary quantitative)
+ Semi-structured interview based on same subject areas but more depth and an exploratory element if any factors or concerns were missed. 5-10 interviews. The structure is in Appendix C (primary qualitative)
+ Theoretical literature review to track dimensions of maturity model present. About 25 sources (secondary qualitative)


The target audience for the survey is Software Product Managers, or at least those that take part in online Product Management discussions. Rather than members of any particular organisation, such as ISPMA. To get a good cross-section of the global industry it will be shared in the following locations: 

+ Mind the Product (2021) Slack channel ~42,000 members
+ Accidental Product Manager LinkedIn (2021a)  group ~35,000 members
+ Product Management LinkedIn (2021b) group ~130,000 members
+ LinkedIn personal post with snowball seeding
+ Product Unleashed (2021) Community 
+ Twitter using #prdmgmt hashtag with snowball seeding
+ Twitter ads 

The snowball sampling (Goodman 2011, Groves 2004) is to attempt to reach practitioners who use general social media but are not as active in specialist forums. A drawback here is that the initial respondents may not be typical as “influencers”, but this should be countered by the more general groups with large membership figures.

The questionnaire will be hosted in Google Forms, with the results stored in Google Sheets to allow for reproducible data analysis using R.


## Code book

| Section  | Question  | Question Type | Column | Coding |  |
|-|-|-|-|-|-|
| Research notification  | (Introductory text about the research) |  |  |  |  |
|  |  I have obtained sufficient information about the study and the processing of my personal data. I have understood the information I have obtained and want to participate in the study.  | Single-answer single choice (checkbox)  | removed from analysis file |  |  |
| About You | Job title | Single-answer multiple choice  + Open answer  | Job.title |  |  |
|  | Are you a member of any of these professional bodies? | Single-answer multiple choice   + Open answer  |  |  |  |
| About where you work | Which industry does your organisation primarily work in? | Single-answer multiple choice  |  |  |  |
|  | What kind of Product do you work on? (Select all that apply) | Multi-answer multiple choice (checkboxes) + Open answer | Split into category indicators | if contains "B2B" product.b2b = True if contains "B2C" product.b2c = True if contains "Mobile" product.mobile =True if contains "Web" product.web = True if contains "Desktop" product.Desktop = True if contains "Service" product.Service = True |  |
|  | Number of employees in your organisation | Single-answer multiple choice  |  |  |  |
|  | Time to market | Single-answer multiple choice  |  |  |  |
|  | Release heart beat | Single-answer multiple choice  |  |  |  |
|  | Product Team Size | Single-answer multiple choice  |  |  |  |
|  | Your primary location | Single-answer multiple choice  |  |  |  |
|  | Our product roadmap helps us deliver our strategy | Single-answer multiple choice (Likert-type scale)  | roadmap.happiness | 1 = "Highly unhappy", 2 ="Unhappy",3 = "Neutral", 4 = "Happy",5 = "Highly happy" |  |
| About your role | Strategic Management | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Product Strategy | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Product Planning | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Development | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Marketing | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Sales & Distribution | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | Service & Support | Multi-answer multiple choice (checkboxes)  + Open answer |  |  |  |
|  | I have appropriate responsibility to achieve my goals | Single-answer multiple choice (Likert-type scale)  | role.happiness | 1 = "Highly unhappy", 2 ="Unhappy",3 = "Neutral", 4 = "Happy",  5 = "Highly happy" |  |
|  About your roadmap | Roadmap detailing - How adequate is your roadmap detailed with respect to the timeline? | Single-answer multiple choice  | roadmap.detailing | clean_responses$roadmap.detailing == "Next steps are planned ad-hoc and there is no mid- to long term planning. Only short-term planning exists."        ~ 1,           clean_responses$roadmap.detailing == "All tasks are planned and worked out in detail for short-,mid- and long term."        ~ 3,           clean_responses$roadmap.detailing == "There is some correlation between time and level of detail but the detailing of the items is not done systematically."        ~ 8,           clean_responses$roadmap.detailing == "There is a clear correlation between time and level of detail. The timelier items are more detailed."        ~ 15,           clean_responses$roadmap.detailing == "Short-term items are detailed, prioritized, estimated and validated. Mid-term items are under validation or being discovered. The long-term timeframe contains themes."        ~ 20 |  |
|  | Roadmap items - Which items are on your product roadmap? | Single-answer multiple choice  | roadmap.item | clean_responses$roadmap.items == "Mainly products"        ~ 1,           clean_responses$roadmap.items == "Mainly products, features"        ~ 3,           clean_responses$roadmap.items == "Mainly business goals, products,  features"        ~ 10,           clean_responses$roadmap.items == "Mainly customer and business goals, products, features and for the long-term timeframe topics (e.g., smart home)"        ~ 12,           clean_responses$roadmap.items == "Mainly product vision, customer and business goals, products, features and for the long-term timeframe themes (i.e., high-level customer needs)"        ~ 20, |  |
|  | Reliability - How often do you adjust your product roadmap? | Single-answer multiple choice  | roadmap.reliability | clean_responses$roadmap.reliability == "Permanent ad-hoc adjustments."        ~ 1,           clean_responses$roadmap.reliability == "Frequent ad-hoc adjustments."        ~ 3,           clean_responses$roadmap.reliability == "Mainly in regular review cycles (e.g., every 3 months)"        ~ 10,           clean_responses$roadmap.reliability == "Adjustments are mainly done reactively on demand."        ~ 10,           clean_responses$roadmap.reliability == "Adjustments are mainly done proactively."        ~ 16 |  |
|  | Confidence - How confident are you that the roadmap items have the expected impact on goals? | Single-answer multiple choice  | roadmap.confidence | clean_responses$roadmap.confidence == "The impacts are not considered."        ~ 1,           clean_responses$roadmap.confidence == "The impacts are mainly estimated by experts"        ~ 4,           clean_responses$roadmap.confidence == "The impacts are mainly determined based on data from the past (e.g., statistics)."        ~ 7,           clean_responses$roadmap.confidence == "The impacts are partly validated"        ~ 10,           clean_responses$roadmap.confidence == "The impacts are systematically validated."        ~ 14 |  |
|  | Discovery – How do you conduct product discovery? | Single-answer multiple choice  | roadmap.discovery | clean_responses$roadmap.discovery == "No discovery activities. Typically, a manager is defining the roadmap items."        ~ 1,           clean_responses$roadmap.discovery == "Product roadmap items are mainly defined based on expert knowledge."        ~ 2,           clean_responses$roadmap.discovery == "Product roadmap items are mainly defined based on customer requests."        ~ 4,           clean_responses$roadmap.discovery == "Several discovery activities are conducted (e.g., user research) but they are not or only loosely integrated with  delivery  activities."        ~ 8,           clean_responses$roadmap.discovery == "Close integration of discovery and delivery activities"        ~ 10 |  |
|  | Responsibility – Who is responsible for placing items on the roadmap? | Single-answer multiple choice  | roadmap.responsibility | clean_responses$roadmap.responsibility == "Tools are used to decide if items are placed on the roadmap (e.g., decision matrix)."        ~ 1,           clean_responses$roadmap.responsibility == "Management"        ~ 2,           clean_responses$roadmap.responsibility == "Specific roles (e.g., portfolio manager)"        ~ 2,           clean_responses$roadmap.responsibility == "Product Management"        ~ 3,           clean_responses$roadmap.responsibility == "Product  Management with cross-functional product teams in  liaison with key stakeholders."        ~ 4 |  |
|  | Prioritization - How do you prioritise items? | Single-answer multiple choice  | roadmap.prioritization | clean_responses$roadmap.prioritization == "First in, first out"        ~ 1,           clean_responses$roadmap.prioritization == "Opinions determine priority"        ~ 2,           clean_responses$roadmap.prioritization == "Prioritization is based on the capability to deliver (e.g., low hanging fruits)"        ~ 3,           clean_responses$roadmap.prioritization == "Prioritization is based on short term benefit (e.g., shareholder value)."        ~ 3,           clean_responses$roadmap.prioritization == "Prioritization is done with an established process and focuses on delivering value to customers and the business."        ~ 7 |  |
|  | Extent of alignment - How do you align your stakeholders? | Single-answer multiple choice  | roadmap.alignment | clean_responses$roadmap.alignment == "No alignment. No one or only one stakeholder such as high level management has a product roadmap that is not communicated to others."        ~ 1,           clean_responses$roadmap.alignment == "Several loosely connected product roadmaps for internal stakeholders exist."        ~ 2,           clean_responses$roadmap.alignment == "Several loosely connected product roadmaps for internal and external stakeholders exist."        ~ 2,           clean_responses$roadmap.alignment == "One central product roadmap exists for different internal and external stakeholders."        ~ 3,           clean_responses$roadmap.alignment == "One central product roadmap exists that allows to derive different representations for different stakeholders. A process for achieving alignment and buy-in is in place."        ~ 5 |  |
|  | Ownership of the product roadmap - Who owns the product roadmap and is accountable? | Single-answer multiple choice | roadmap.ownership | clean_responses$roadmap.ownership == "No owner defined"        ~ 1,           clean_responses$roadmap.ownership == "Managers"        ~ 2,           clean_responses$roadmap.ownership == "Ownership is shared between multiple roles"        ~ 3,           clean_responses$roadmap.ownership == "Strategy or portfolio planning"        ~ 3,           clean_responses$roadmap.ownership == "Product management or product teams"        ~ 4 |  |
|  | Tools - Which tool(s) do you use to manage your roadmap process? | Multi-answer multiple choice (checkboxes) + Open answer | roadmap.tools |  |  |
| Finally | Where do you look for information about Software Product Management? (tick all that apply) | Multi-answer multiple choice (checkboxes) + Open answer | info.sources |  |  |
|  | Is there anything else you'd like to say about product management, strategy, and innovation in your working life? (the answers to this question will not be in the publicly available data set, and only published in aggregate analysis or quoted in an anonymised form) | Open answer | removed from analysis file |  |  |