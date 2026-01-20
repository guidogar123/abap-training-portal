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
                    title: "S4: Sintaxis y Eventos",
                    content: `
### Sintaxis ABAP Básica
ABAP no distingue entre mayúsculas y minúsculas (excepto literales).

**Eventos de un Reporte**:
1.  **INITIALIZATION**: Seteo de valores iniciales.
2.  **AT SELECTION-SCREEN**: Validación de pantalla.
3.  **START-OF-SELECTION**: Lógica central (Selects).
4.  **END-OF-SELECTION**: Finalización/ALV.
            `,
                    examples: [
                        {
                            language: "abap",
                            code: `
REPORT z_ejemplo.
INITIALIZATION.
  WRITE 'Inicio'.
START-OF-SELECTION.
  WRITE 'Ejecución'.
                    `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 5,
                    title: "W1: Taller Semanal - Mi Primera Tabla y Reporte",
                    content: `
### Objetivo
Integrar DDIC y Sintaxis en un mini-proyecto.

### Tareas
1. **SE11**: Crear tabla \`ZESTUDIANTES\` con ID, Nombre y Fecha Ingreso.
2. **SE38**: Crear reporte que lea la tabla y valide que el ID no sea cero en \`AT SELECTION-SCREEN\`.
3. **Eventos**: Usar \`INITIALIZATION\` para sugerir la fecha de hoy.
          `,
                    examples: [],
                    exercises: []
                }
            ]
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
### SELECT & Performance
**Reglas de Oro:**
1. Filtrar por Clave/Índices.
2. NUNCA SELECT dentro de un LOOP.
3. Usar \`FOR ALL ENTRIES\` para combinar tablas.
          `,
                    examples: [
                        {
                            language: "abap",
                            code: `
SELECT kunnr name1 FROM kna1 
  INTO TABLE lt_clientes
  FOR ALL ENTRIES IN lt_pedidos
  WHERE kunnr = lt_pedidos-kunnr.
              `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 7,
                    title: "S7: Modularización",
                    content: `
### Reutilización de Código
- **Subrutinas (PERFORM)**: Locales al programa.
- **Módulos de Función (SE37)**: Globales y reutilizables.
- **Métodos (OO)**: El estándar moderno.
          `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 8,
                    title: "S8: Reportes ALV (SALV)",
                    content: `
### CL_SALV_TABLE
Es la clase moderna para mostrar grillas de datos con funciones integradas (filtros, excel).
          `,
                    examples: [
                        {
                            language: "abap",
                            code: `
cl_salv_table=>factory(
  IMPORTING r_salv_table = lo_alv
  CHANGING  t_table      = lt_datos ).
lo_alv->display( ).
              `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 9,
                    title: "W2: Taller Semanal - Reporte Profesional ALV",
                    content: `
### Objetivo
Crear un reporte que una Cabecera de Ventas (\`VBAK\`) y Clientes (\`KNA1\`).

### Tareas
1. Realizar la consulta usando \`FOR ALL ENTRIES\`.
2. Modularizar la lógica en una clase local o subrutinas.
3. Mostrar el resultado usando \`CL_SALV_TABLE\`.
          `,
                    examples: [],
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
                    title: "S11: Batch Input & SHDB",
                    content: `
### Concepto
Simular la interacción del usuario en una transacción para cargar datos masivos.
          `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 12,
                    title: "S12: BAPIs",
                    content: `
### Business API
Funciones estándar para realizar transacciones de negocio asegurando integridad. Requieren \`BAPI_TRANSACTION_COMMIT\`.
          `,
                    examples: [
                        {
                            language: "abap",
                            code: `
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
  EXPORTING order_header_in = ls_header
  TABLES return = lt_return.
              `
                        }
                    ],
                    exercises: []
                },
                {
                    id: 13,
                    title: "W3: Taller Semanal - Carga de Pedidos",
                    content: `
### Objetivo
Implementar una carga de pedidos de venta.

### Tareas
1. Crear un reporte que tome una lista de pedidos.
2. Procesar cada uno usando la BAPI de creación de pedidos.
3. Mostrar un log de errores/éxitos al final.
          `,
                    examples: [],
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
                    title: "S16: User Exits & BAdIs",
                    content: `
### Ampliaciones
Puntos donde podemos insertar código en el estándar sin modificar el core de SAP.
- **BAdI**: Orientadas a Objetos (SE18/SE19).
- **User Exits**: Subrutinas en includes específicos.
          `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 17,
                    title: "S17: Enhancement Framework",
                    content: `
### Implicit & Explicit Enhancements
Permiten "inyectar" código al inicio o final de casi cualquier función o método estándar.
          `,
                    examples: [],
                    exercises: []
                },
                {
                    id: 18,
                    title: "W4: Taller Final - Validación de Materiales",
                    content: `
### Objetivo
Implementar una regla de negocio en una transacción estándar.

### Tareas
1. Buscar una BAdI en la transacción \`MM01\`.
2. Implementar una validación que impida guardar si falta el peso del material.
3. Probar la validación y capturar el mensaje de error.
          `,
                    examples: [],
                    exercises: []
                }
            ]
        }
    ]
};
