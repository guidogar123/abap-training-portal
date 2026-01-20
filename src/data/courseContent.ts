export interface Example {
    language: string;
    code: string;
}

export interface Session {
    id: number;
    title: string;
    content: string;
    examples?: Example[];
    exercises?: string[];
}

export interface Week {
    id: number;
    title: string;
    description?: string;
    sessions: Session[];
}

export interface CourseData {
    title: string;
    description: string;
    weeks: Week[];
}

export const courseData: CourseData = {
    title: "Master en SAP ABAP - De Cero a Experto",
    description: "Curso intensivo de 4 semanas para dominar el desarrollo en SAP ECC.",
    weeks: [
        {
            id: 1,
            title: "Semana 1: Fundamentos y Diccionario de Datos",
            description: "Entendiendo la arquitectura SAP y el manejo de datos.",
            sessions: [
                {
                    id: 1,
                    title: "S1: Arquitectura y Navegación",
                    content: `
### Arquitectura SAP (R/3 - ECC)
SAP opera bajo una arquitectura cliente-servidor de 3 capas:
1.  **Capa de Presentación (SAP GUI / Fiori)**: La interfaz con la que interactúa el usuario.
2.  **Capa de Aplicación**: Donde reside la lógica de negocio y se ejecutan los programas ABAP. Aquí están los Work Processes (Dialog, Background, Update, etc.).
3.  **Capa de Base de Datos**: Donde se almacenan los datos (HANA, Oracle, SQL Server).

### Navegación Básica
Las transacciones son atajos a programas.
- **/o**: Abre una nueva sesión.
- **/n**: Finaliza la transacción actual.
- **Transacciones Core**:
    - **SE80**: Object Navigator (El IDE principal).
    - **SE38**: Editor ABAP (para reportes simples).
    - **SE11**: Diccionario de Datos.
    - **SE37**: Biblioteca de Funciones.
          `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 2,
                    title: "S2: Diccionario de Datos (DDIC) I",
                    content: `
### Objetos del Diccionario
El DDIC (SE11) es el corazón de SAP. Todos los datos se definen aquí.

1.  **Tablas Transparentes**: Tienen una correspondencia 1:1 con una tabla física en la BDD.
2.  **Elementos de Datos**: Definen la *semántica* (etiquetas, documentación, ayuda F1) y el tipo.
3.  **Dominios**: Definen las *características técnicas* (CHAR, NUMC, longitud, valores permitidos).

**Jerarquía**: Campo de Tabla -> Elemento de Datos -> Dominio.
          `,
                    examples: [
                        {
                            language: "abap",
                            code: `
* No se programa, se configura en SE11.
* Ejemplo de Jerarquía:
* Tabla: ZTABLA_VENTAS
*   Campo: VKORG (Org. Ventas)
*     -> Elemento de Datos: VKORG (Descripción: "Organización de Ventas")
*       -> Dominio: VKORG (Tipo: CHAR, Longitud: 4)
              `
                        }
                    ],
                    exercises: [
                        "Crear Dominio 'ZDOM_STATUS' (CHAR 1) con valores fijos: 'A' (Activo), 'X' (Cancelado).",
                        "Crear Elemento de Datos 'ZDT_STATUS' asignado al dominio anterior.",
                        "Crear Tabla Transparente 'ZTAB_PEDIDOS' con campos: Mandante (MANDT), ID Pedido (Key), Fechas y el Status creado."
                    ]
                },
                {
                    id: 3,
                    title: "S3: Diccionario de Datos II",
                    content: `
### Estructuras vs Tablas
- **Estructura**: Es un tipo de dato complejo que existe solo en tiempo de ejecución (no guarda datos en BD). Se usa para definir variables en programas o parámetros de interfaces.
- **Vistas de Base de Datos**: Joins predefinidos entre tablas para facilitar lecturas.

### Tablas Internas en ABAP
Son arrays en memoria. Es vital entender los 3 tipos:
1.  **STANDARD TABLE**: Búsqueda lineal (vía índice). Rápida inserción.
2.  **SORTED TABLE**: Siempre ordenada por clave. Búsqueda binaria automática.
3.  **HASHED TABLE**: Búsqueda por algoritmo hash (O(1)). Ideal para grandes volúmenes y lecturas por clave única.
            `,
                    examples: [
                        {
                            language: "abap",
                            code: `
* Definición de tipos
TYPES: BEGIN OF ty_pedido,
         vbeln TYPE vbeln_va,
         erdat TYPE erdat,
       END OF ty_pedido.

* Tabla Interna Estándar
DATA: lt_pedidos TYPE STANDARD TABLE OF ty_pedido.

* Tabla Hashed (Lectura ultra rápida)
DATA: lt_kna1 TYPE HASHED TABLE OF kna1 
      WITH UNIQUE KEY kunnr.
                    `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 4,
                    title: "S4: Sintaxis y Programación Orientada a Eventos",
                    content: `
### Sintaxis ABAP Básica
ABAP no distingue entre mayúsculas y minúsculas (excepto en literales de texto). Cada sentencia debe terminar en un punto (\`.\`).

**Encadenamiento de Sentencias (Chain Statements)**:
Se usa el dos puntos (\`:\`) para agrupar comandos que comparten el inicio.
\`\`\`abap
WRITE: 'Hola', 'Mundo', 'SAP'.
\`\`\`

**Convenciones de Nomenclatura Recomendadas**:
- \`lv_...\`: Variable local.
- \`gv_...\`: Variable global.
- \`lt_...\`: Tabla interna local.
- \`ls_...\`: Estructura (Line) local.

### Programación Orientada a Eventos (Reporting)
A diferencia de otros lenguajes lineales, los reportes ABAP se ejecutan según el ciclo de vida del entorno SAP, activando eventos específicos:

1.  **LOAD-OF-PROGRAM**: Se activa al cargar el programa en memoria.
2.  **INITIALIZATION**: Momento ideal para setear valores por defecto en la pantalla de selección.
3.  **AT SELECTION-SCREEN**: Validación de los datos ingresados por el usuario. Aquí se disparan los mensajes de error.
4.  **START-OF-SELECTION**: El evento principal. Aquí reside la lógica de negocio (consultas a base de datos).
5.  **END-OF-SELECTION**: Se ejecuta después de la lógica principal. Ideal para mostrar resultados o llamar al ALV.
            `,
                    examples: [
                        {
                            language: "abap",
                            code: `
REPORT z_ejemplo_eventos.

*--------------------------------------------------------------------*
* DECLARACIÓN DE DATOS
*--------------------------------------------------------------------*
TABLES: mara.

SELECT-OPTIONS: s_matnr FOR mara-matnr. " Pantalla de selección
PARAMETERS: p_check AS CHECKBOX DEFAULT 'X'.

DATA: lt_materiales TYPE TABLE OF mara,
      ls_material   TYPE mara.

*--------------------------------------------------------------------*
* EVENTOS
*--------------------------------------------------------------------*

INITIALIZATION.
  " Este código corre ANTES de que el usuario vea la pantalla
  APPEND VALUE #( sign = 'I' option = 'BT' low = '1' high = '100' ) TO s_matnr.

AT SELECTION-SCREEN.
  " Validación de campos
  IF s_matnr[] IS INITIAL AND p_check = 'X'.
    MESSAGE 'Favor de ingresar al menos un material' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  " Lógica principal: Lectura de base de datos
  SELECT * FROM mara 
    INTO TABLE lt_materiales
    WHERE matnr IN s_matnr.

END-OF-SELECTION.
  " Visualización de resultados
  IF lt_materiales IS NOT INITIAL.
    LOOP AT lt_materiales INTO ls_material.
      WRITE: / ls_material-matnr, ls_material-matart.
    ENDLOOP.
  ELSE.
    WRITE: 'No se encontraron resultados.'.
  ENDIF.
                            `
                        }
                    ],
                    exercises: [
                        "Crear un reporte que use INITIALIZATION para poner la fecha de hoy en un parámetro.",
                        "En AT SELECTION-SCREEN, validar que un campo de usuario no esté vacío.",
                        "Implementar START-OF-SELECTION para realizar una suma y END-OF-SELECTION para mostrarla."
                    ]
                }
            ] // Fin sesiones semana 1
        },
        {
            id: 2,
            title: "Semana 2: Consultas, Modularización y ALV",
            description: "Extracción eficiente de datos y presentación profesional.",
            sessions: [
                {
                    id: 6,
                    title: "S6: Open SQL Avanzado",
                    content: `
### SELECT
La sentencia para leer base de datos.
**Reglas de Oro del Performance:**
1.  **SIEMPRE** filtrar por Clave Primaria o Indices si es posible.
2.  **NUNCA** hacer SELECT dentro de un LOOP. (N+1 Problem).
3.  Seleccionar solo los campos necesarios (Evitar \`SELECT *\`).

### FOR ALL ENTRIES
Para unir datos en memoria cuando un JOIN es muy complejo o lento. Equivale a un \`WHERE IN (...)\` masivo.
          `,
                    examples: [
                        {
                            language: "abap",
                            code: `
* Mala práctica (Select inside Loop)
LOOP AT lt_pedidos INTO ls_pedido.
  SELECT SINGLE * FROM kna1 INTO ls_kna1 
    WHERE kunnr = ls_pedido-kunnr. "MALO!!
ENDLOOP.

* Buena práctica (For All Entries)
IF lt_pedidos IS NOT INITIAL.
  SELECT kunnr name1 
    FROM kna1 
    INTO TABLE lt_clientes
    FOR ALL ENTRIES IN lt_pedidos
    WHERE kunnr = lt_pedidos-kunnr.
ENDIF.
              `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 9,
                    title: "S9: Reportes ALV",
                    content: `
### ABAP List Viewer (ALV)
Es la grilla estándar de SAP. Permite ordenar, filtrar, exportar a Excel, etc.

#### Opciones:
1.  **Posición de Funciones (Legacy)**: \`REUSE_ALV_GRID_DISPLAY\`. Fácil pero obsoleta.
2.  **POO (Moderna)**: \`CL_SALV_TABLE\`. Mucho más limpia y orientada a objetos (Factory pattern).
            `,
                    examples: [
                        {
                            language: "abap",
                            code: `
* Uso de CL_SALV_TABLE
DATA: lo_alv TYPE REF TO cl_salv_table.

TRY.
    cl_salv_table=>factory(
      IMPORTING r_salv_table = lo_alv
      CHANGING  t_table      = lt_datos ).
    
    lo_alv->display( ).
    
  CATCH cx_salv_msg.
ENDTRY.
                    `
                        }
                    ],
                    exercises: []
                }
            ]
        },
        {
            id: 3,
            title: "Semana 3: Integración (Batch Input & BAPIs)",
            description: "Carga masiva e interfaces.",
            sessions: [
                {
                    id: 11,
                    title: "S11: Batch Input (LSMW / SHDB)",
                    content: `
### Concepto
Simular que un usuario teclea datos en una transacción.
Usamos la Tx **SHDB** para grabar los pasos y obtener el código.

### Comandos
- \`BDC_OPEN_GROUP\`: Iniciar sesión.
- \`BDC_INSERT\`: Insertar transacción.
- \`CALL TRANSACTION 'VA01' USING lt_bdcdata MODE 'N' UPDATE 'S'.\`
  - MODE 'N': No mostrar pantallas (background).
  - MODE 'A': Mostrar todas (debug).
                `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 12,
                    title: "S12: BAPIs (Business Application Programming Interfaces)",
                    content: `
Las BAPIs son métodos RFC estandarizados por SAP que garantizan la integridad de los datos. Son preferibles al Batch Input.
Se buscan en la Tx **BAPI**.

Siempre requieren un \`COMMIT WORK\` explícito al final si son de escritura.
                `,
                    examples: [
                        {
                            language: "abap",
                            code: `
DATA: ls_header  TYPE bapisdhd1,
      lt_items   TYPE TABLE OF bapisditm,
      lt_return  TYPE TABLE OF bapiret2.

ls_header-doc_type = 'TA'.
ls_header-sales_org = '1000'.

CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING
    order_header_in = ls_header
  TABLES
    return          = lt_return
    order_items_in  = lt_items.

IF line_exists( lt_return[ type = 'E' ] ).
  ROLLBACK WORK.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDIF.
                        `
                        }
                    ],
                    exercises: []
                }
            ]
        },
        {
            id: 4,
            title: "Semana 4: Ampliaciones y Formularios",
            description: "Modificando el estándar SAP de forma segura.",
            sessions: [
                {
                    id: 16,
                    title: "S16: User Exits & Customer Exits",
                    content: `
### User Exits (Form Exits)
Son los más antiguos. Son subrutinas (\`FORM\`) vacías dentro de programas estándar (generalmente en SD).
- **Ejemplo**: Programa \`SAPMV45A\`, Include \`MV45AFZZ\`.
- **Riesgo**: Requieren clave de acceso (en versiones viejas) y múltiples desarrolladores pueden colisionar si editan el mismo include.

### Customer Exits (Function Exits)
Gestionados en Tx **SMOD** (Definición) y **CMOD** (Implementación).
Son Módulos de Función con nombres como \`EXIT_SAPMV45A_002\`.
Dentro tienen un Z-Include (ej. \`ZX...\`) donde pones tu código.
                `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 17,
                    title: "S17: BAdIs (Business Add-Ins)",
                    content: `
La evolución orientada a objetos de las ampliaciones.
- **Classic BAdIs**: Se basan en clases ABAP.
- **New BAdIs (Kernel)**: Más rápidas, almacenadas en el Kernel. Tx **SE18** (Definición) y **SE19** (Implementación).
- Soportan múltiples implementaciones (a diferencia de los User Exits).

### ¿Cómo encontrar una BAdI?
1.  Poner un breakpoint en el método \`cl_exithandler=>get_instance\`.
2.  Ejecutar la transacción estándar.
3.  Cada vez que pare, mirar la variable \`exit_name\`.
                `,
                    examples: [
                        {
                            language: "abap",
                            code: `
* En el método GET_INSTANCE:
CASE exit_name.
  WHEN 'BADI_MATERIAL_CHECK'.
    " Aquí es donde SAP busca la implementación
ENDCASE.
                        `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 18,
                    title: "S18: Enhancement Framework",
                    content: `
Es la tecnología más potente y moderna. Permite modificar código estándar *sin* modificar el objeto original (técnicamente).

### Tipos:
1.  **Implicit Enhancement Points**: Puntos predefinidos por SAP al inicio y final de CADA subrutina, función o método. ¡Están en todas partes!
    - *Cómo verlos*: En SE38 -> Editar -> Menu: Edit -> Enhancement Operations -> Show Implicit Enhancement Options.
2.  **Explicit Enhancement Points**: Puntos creados manualmente por SAP en su código (\`ENHANCEMENT-POINT\`).

### BTEs (Business Transaction Events)
Específicos de Finanzas (FI). Usan la transacción **FIBF**. Funcionan como un patrón Publish/Subscribe.
                `,
                    examples: [],
                    exercises: []
                }
            ]
        }
    ]
};
