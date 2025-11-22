# Projects

Scientific research is not only about data collection and analysis. It also involves **management and bureaucracy**, such as defining objectives, timelines, budgets, and responsibilities. In Xolmis, these aspects are formalized in **Projects**, which allow you to organize research activities, track goals, manage funding, and monitor progress.

Open the Projects module in the main menu: **Management → Projects**.

## Adding and editing a project

When creating or editing a project, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Title** | Yes | Full title of the project |
| **Short title** | Yes | Abbreviated title used when the full title is too long |
| **Protocol nr.** |  | Official protocol or identification number of the project |
| **Start date** |  | First day of the project |
| **End date** |  | Last day of the project |
| **Website** |  | Project website address |
| **E-mail** |  | Contact e-mail address of the project or responsible person |
| **Contact person** |  | Person responsible for answering inquiries |
| **Main goal** |  | Description of the main goal of the project |
| **Risks** |  | Risks or challenges that need to be considered |
| **Abstract** |  | Project abstract or summary |
| **Notes** |  | Any other information about the project |

## Project members

Projects usually involve multiple researchers and institutions. You can register members with their roles and affiliations:

| Field | Required | Description |
| --- | --- | --- |
| **Researcher** | Yes | Person participating in the project, selected from the Researchers table |
| **Project manager** |  | Indicates if the researcher is the project manager |
| **Institution** |  | Institution to which the researcher is affiliated |

## Goals

Goals represent the **expected outcomes** of the project. They help track progress and evaluate success.

| Field | Required | Description |
| --- | --- | --- |
| **Description** | Yes | Goal description |
| **Status** | Yes | Status of the goal: Pending, Reached, Canceled |

## Chronogram

The chronogram defines the **activities and timeline** of the project. Each activity can be linked to a goal, making it easier to monitor progress.

| Field | Required | Description |
| --- | --- | --- |
| **Description** | Yes | Activity description |
| **Status** | Yes | Status of the activity: To do, In progress, Finished, Canceled, Delayed, Needs review, Blocked |
| **Start date** |  | Date when the activity started or will start |
| **Target date** |  | Expected completion date |
| **End date** |  | Date when the activity was finished |
| **Goal** |  | Goal to which the activity is linked |

## Budget

The budget section allows you to record **funding sources and allocations**. This helps track how resources are planned and distributed.

| Field | Required | Description |
| --- | --- | --- |
| **Funding source** | Yes | Entity or person funding the project |
| **Rubric** | Yes | Budget category or rubric |
| **Item** |  | Specific item within the rubric |
| **Amount** |  | Amount of funding for this rubric/item |

## Expenses

Expenses represent the **actual costs** incurred during the project. They are linked to rubrics defined in the budget, ensuring consistency between planned and executed spending.

| Field | Required | Description |
| --- | --- | --- |
| **Rubric** | Yes | Budget rubric to which the expense is linked |
| **Item description** |  | Description of the expense |
| **Date** |  | Date of the expense |
| **Amount** |  | Amount of the expense |

## Best practices

- **Define clear goals**: Goals should be specific, measurable, and linked to activities.  
- **Keep the chronogram updated**: Regularly update activity statuses to monitor progress.  
- **Track funding and expenses**: Ensure that expenses are linked to rubrics to maintain financial transparency.  
- **Assign responsibilities**: Identify project managers and contact persons to streamline communication.  
- **Document risks**: Recording risks helps anticipate problems and plan mitigation strategies.  
- **Use notes and abstracts**: Provide summaries and contextual information for easier understanding by collaborators.  

## Relation to other modules

Projects are interconnected with other parts of Xolmis:

- **[Researchers](researchers.md)**: Members are selected from the Researchers table.  
- **[Institutions](institutions.md)**: Affiliations are linked to registered institutions.  
- **[Permits](permits.md)**: Projects often require permits, which can be managed in the Permits module.  
- **[Captures](captures.md) and [Specimens](specimens.md)**: Data collected in the field can be associated with specific projects.  
- **[Reports](print-data.md)**: Project data can be summarized and exported for funding agencies or institutional review.  

By managing projects in Xolmis, you ensure that research activities are organized, traceable, and compliant with administrative requirements.
