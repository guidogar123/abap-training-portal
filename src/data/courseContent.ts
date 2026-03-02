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
    title: "Master en SAP ABAP - Sistemas e Información S.A.S",
    description: "Programa intensivo de 4 semanas para consultores. 4 horas diarias de inmersión técnica.",
    weeks: [
        {
            id: 1,
            title: "Semana 1: Core ABAP y Diccionario de Datos",
            description: "Dominio de la base de datos SAP y lógica procedural fundacional.",
            sessions: [
                {
                    id: 1,
                    title: "S1: Arquitectura SAP y Ecosistema de Desarrollo",
                    content: `### Arquitectura R/3 y ECC
La base del desarrollo ABAP reside en entender dónde se ejecuta el código. 
- **Work Processes**: Dialog, Background, Update, Spool.
- **Transacciones Clave**: SE80 (Object Navigator), SE11 (DDIC), SE38 (Editor).`,
                    exercises: [
                        "Identificar las capas de arquitectura en un servidor SAP real.",
                        "Navegar entre componentes de un paquete estándar en SE80.",
                        "Configurar el editor ABAP (Pretty Printer) según estándares SEI.",
                        "Diferenciar entre mandantes transitivos y no transitivos.",
                        "Explicar el ciclo de vida de un Work Process de Diálogo."
                    ]
                },
                {
                    id: 2,
                    title: "S2: DDIC I - Tablas, Dominios y Elementos",
                    content: `### Diccionario de Datos (SE11)
El corazón de SAP. La jerarquía Campo -> Elemento -> Dominio asegura integridad semántica y técnica.`,
                    exercises: [
                        "Crear un Dominio de estados con valores fijos.",
                        "Definir un Elemento de Datos con documentación F1 personalizada.",
                        "Crear una Tabla Transparente con campos clave y mandante.",
                        "Configurar las especificaciones técnicas (Clase de datos y categoría de tamaño).",
                        "Realizar mantenimiento de registros mediante utilidades de tabla."
                    ]
                },
                {
                    id: 3,
                    title: "S3: DDIC II - Vistas, Estructuras y Ayudas F4",
                    content: `### Objetos Complejos
Optimización de búsquedas y visualización de datos relacionados.`,
                    exercises: [
                        "Crear una Ayuda de Búsqueda Elemental (Search Help).",
                        "Diseñar una Ayuda de Búsqueda Colectiva que combine múltiples fuentes.",
                        "Definir una Database View uniendo 3 tablas estándar.",
                        "Crear una Estructura Z para el paso de parámetros en funciones.",
                        "Vincular una ayuda de búsqueda a un campo de tabla sin usar elementos de datos."
                    ]
                },
                {
                    id: 4,
                    title: "S4: Lógica Procedural y Manejo de Tablas Internas",
                    content: `### El Motor de ABAP
Manejo eficiente de datos en memoria usando Standard, Sorted y Hashed tables.`,
                    exercises: [
                        "Diferenciar rendimiento entre READ TABLE con BINARY SEARCH vs Hashed access.",
                        "Implementar un LOOP con cláusula WHERE vs bloque CONDENSED.",
                        "Uso de Field Symbols para modificar registros en tiempo real.",
                        "Operaciones CRUD en tablas internas (INSERT, MODIFY, DELETE).",
                        "Manejo de cabeceras (Header Lines) vs Estructuras de Trabajo (Work Areas)."
                    ]
                },
                {
                    id: 5,
                    title: "S5: Modularización y Reutilización",
                    content: `### Clean Code ABAP
Subrutinas, Grupos de Funciones y la importancia de la encapsulación.`,
                    exercises: [
                        "Crear un Grupo de Funciones Z para cálculos financieros.",
                        "Implementar un Módulo de Función con parámetros opcionales y excepciones.",
                        "Diferenciar entre llamada por referencia y llamada por valor.",
                        "Uso de Includes para organizar programas extensos.",
                        "Crear un programa que llame a una función remota (RFC)."
                    ]
                }
            ]
        },
        {
            id: 2,
            title: "Semana 2: OO ABAP y Experiencia de Usuario",
            description: "Transición del modelo procedural al orientado a objetos y reportes dinámicos.",
            sessions: [
                {
                    id: 6,
                    title: "S6: Open SQL Moderno y Performance",
                    content: `### Consultas de Alto Rendimiento
Comparativa entre Open SQL y SQL Nativo. El uso de INNER JOIN vs FOR ALL ENTRIES.`,
                    exercises: [
                        "Refactorizar un SELECT en un LOOP usando FOR ALL ENTRIES.",
                        "Implementar agregaciones (SUM, COUNT, AVG) directamente en el SELECT.",
                        "Uso de Case Expressions dentro de consultas SQL.",
                        "Validar la nulidad de tablas antes de consultas masivas.",
                        "Análisis de trazas SQL mediante la transacción ST05."
                    ]
                },
                {
                    id: 7,
                    title: "S7: Programación Orientada a Objetos (OO ABAP) I",
                    content: `### Clases e Instancias
Definición, Implementación y visibilidad (Public, Protected, Private).`,
                    exercises: [
                        "Crear una clase local con métodos estáticos e instanciables.",
                        "Implementar el concepto de Constructores y Destructores.",
                        "Manejo de Atributos de lectura única (READ-ONLY).",
                        "Uso de clases globales en SE24.",
                        "Modelar un objeto 'Pedido' con validaciones internas."
                    ]
                },
                {
                    id: 8,
                    title: "S8: OO ABAP II - Herencia, Interfaces y Polimorfismo",
                    content: `### Patrones de Diseño
Cómo crear código flexible que soporte el crecimiento de la lógica de negocio.`,
                    exercises: [
                        "Crear una Interface para estandarizar procesos de LOG.",
                        "Implementar Herencia entre clases de diferentes tipos de materiales.",
                        "Uso de Casting (Up-Casting y Down-Casting) seguro.",
                        "Implementar el patrón de diseño Singleton.",
                        "Desarrollar una factoría de clases (Factory Pattern) dinámica."
                    ]
                },
                {
                    id: 9,
                    title: "S9: Reportes ALV con CL_SALV_TABLE",
                    content: `### Visualización Profesional
Uso del framework SALV para mostrar datos de forma tabular con filtros y exportación.`,
                    exercises: [
                        "Generar un reporte básico con CL_SALV_TABLE.",
                        "Personalizar el catálogo de campos (Títulos y visibilidad).",
                        "Implementar funciones de agregación y ordenamiento por código.",
                        "Añadir botones personalizados en la barra de herramientas del ALV.",
                        "Hacer que el ALV sea jerárquico o tipo Tree."
                    ]
                },
                {
                    id: 10,
                    title: "S10: Dynpros e Interfaces Gráficas Tradicionales",
                    content: `### Screen Painter
Diseño de pantallas personalizadas, manejo de PBO (Process Before Output) y PAI (Process After Input).`,
                    exercises: [
                        "Diseñar una pantalla con campos de entrada y botones en SE51.",
                        "Manejo de Table Controls para entrada masiva de datos en Dynpros.",
                        "Implementar validaciones de campo en el evento PAI.",
                        "Uso de pestañas (Tabstrips) dinámicas.",
                        "Integrar una ventana emergente (Popup) modal personalizada."
                    ]
                }
            ]
        },
        {
            id: 3,
            title: "Semana 3: Integración, BAPIs y Extensibilidad",
            description: "Comunicación entre sistemas y modificación de procesos estándar.",
            sessions: [
                {
                    id: 11,
                    title: "S11: Carga de Datos y Batch Input",
                    content: `### SHDB y Transacciones
Automatización de transacciones estándar mediante grabaciones de pantalla.`,
                    exercises: [
                        "Realizar una grabación de la transacción MM01 (Materiales).",
                        "Crear un programa que procese un archivo plano mediante Call Transaction.",
                        "Manejo de errores y logs en procesamientos de fondo.",
                        "Simular errores de pantalla y recuperarlos en el programa.",
                        "Diferenciar entre modo 'A', 'N' y 'E' en Batch Input."
                    ]
                },
                {
                    id: 12,
                    title: "S12: BAPIs y Commits Transaccionales",
                    content: `### El Estándar de Integración
Uso de Business APIs para crear documentos financieros y logísticos con integridad total.`,
                    exercises: [
                        "Crear un Pedido de Compras usando BAPI_PO_CREATE1.",
                        "Implementar el manejo del buffer transaccional (BAPI_TRANSACTION_COMMIT).",
                        "Interpretar la tabla RETURN para reportes de errores.",
                        "Utilizar extensiones BAPI (ExtensionIn) para campos Z.",
                        "Realizar una modificación masiva de precios con BAPI_MATERIAL_SAVEDATA."
                    ]
                },
                {
                    id: 13,
                    title: "S13: User Exits y Customer Exits",
                    content: `### Modificando el Estándar (Legacy)
Identificación y uso de puntos de ampliación clásicos en módulos SD, MM y FI.`,
                    exercises: [
                        "Localizar un User Exit en el programa de ventas MV45AFZZ.",
                        "Añadir una validación de crédito en el guardado del pedido.",
                        "Implementar un Customer Exit (SMOD/CMOD).",
                        "Pasar datos de una pantalla estándar a una tabla Z.",
                        "Depurar un User Exit activo del estándar."
                    ]
                },
                {
                    id: 14,
                    title: "S14: Business Add-Ins (BAdIs)",
                    content: `### Extensibilidad Orientada a Objetos
El estándar actual de ampliaciones SAP (Classic y Kernel BAdIs).`,
                    exercises: [
                        "Implementar una BAdI estándar para cálculo de impuestos.",
                        "Crear una BAdI propia (Z) para procesos personalizados.",
                        "Diferenciar tipos de implementación (Single vs Multiple Use).",
                        "Uso de filtros en la implementación de BAdIs.",
                        "Migración de Classic BAdI a New Enhancement Framework."
                    ]
                },
                {
                    id: 15,
                    title: "S15: Enhancement Framework (Implicit/Explicit)",
                    content: `### Inyecciones de Código
Uso de Enhancement Points y Sections para modificar cualquier parte del sistema.`,
                    exercises: [
                        "Crear un Enhancement Implícito al final de una función estándar.",
                        "Implementar un Enhancement de Clase para añadir métodos a clases estándar.",
                        "Uso de Enhancement Sections para reemplazar lógica estándar.",
                        "Gestionar capas de implementación (Switch Framework).",
                        "Documentar y organizar las ampliaciones para auditorías técnicas."
                    ]
                }
            ]
        },
        {
            id: 4,
            title: "Semana 4: El Futuro - ABAP on HANA y Cloud",
            description: "Modernización hacia S/4HANA, CDS, RAP y Servicios OData.",
            sessions: [
                {
                    id: 16,
                    title: "S16: ABAP sobre SAP HANA",
                    content: `### El Paradigma Code-to-Data
Optimización de código para aprovechar la potencia de la base de datos in-memory.`,
                    exercises: [
                        "Uso de Expresiones Aritméticas y de String en Open SQL.",
                        "Crear procedimientos AMDP (ABAP Managed Database Procedures).",
                        "Comparativa de performance: Select Tradicional vs HANA Optimized SQL.",
                        "Uso de la herramienta ATC (ABAP Test Cockpit) para Readiness Check.",
                        "Consumo de funciones nativas de HANA desde ABAP."
                    ]
                },
                {
                    id: 17,
                    title: "S17: Core Data Services (CDS Views)",
                    content: `### Modelado de Datos Moderno
Creación de modelos de datos enriquecidos con anotaciones en la capa de persistencia.`,
                    exercises: [
                        "Crear una CDS View básica con proyecciones y renombrado de campos.",
                        "Implementar CDS con Joins y Uniones complejas.",
                        "Uso de Anotaciones para UI y Search-ability.",
                        "Crear una CDS con Parámetros de entrada.",
                        "Consumir una CDS View desde un programa ABAP tradicional."
                    ]
                },
                {
                    id: 18,
                    title: "S18: SAP Gateway y Servicios OData",
                    content: `### Conectividad Fiori
Exponer la lógica ABAP al mundo exterior mediante protocolos RESTful y la transacción SEGW.`,
                    exercises: [
                        "Modelar un servicio OData en la transacción SEGW.",
                        "Implementar las operaciones CRUD (Create, Read, Update, Delete) en la clase DPC_EXT.",
                        "Probar el servicio mediante el Gateway Client.",
                        "Manejo de filtros y expansión de entidades ($expand).",
                        "Añadir validaciones de negocio en el servicio OData."
                    ]
                },
                {
                    id: 19,
                    title: "S19: ABAP RESTful Application Programming (RAP)",
                    content: `### El Nuevo Estándar
Desarrollo de principio a fin para la nube y S/4HANA Cloud.`,
                    exercises: [
                        "Crear un Business Configuration (BC) siguiendo el modelo RAP.",
                        "Definir el Behavior Definition para una entidad.",
                        "Implementar acciones personalizadas (Actions) en RCP.",
                        "Uso de Proyecciones de Comportamiento (Behavior Projections).",
                        "Desplegar una aplicación Fiori Elements basada en un servicio RAP."
                    ]
                },
                {
                    id: 20,
                    title: "S20: Clean ABAP y Unit Testing",
                    content: `### Calidad y Mantenibilidad
Principios de Clean ABAP, ABAP Unit y desarrollo TDD (Test Driven Development).`,
                    exercises: [
                        "Escribir una Clase de Test (Unit Test) para una lógica compleja.",
                        "Uso de Inyección de Dependencias para testing.",
                        "Aplicar principios de nombrado y estructuración de Clean ABAP.",
                        "Medir la cobertura de tests mediante el Coverage Tool.",
                        "Refactorizar código legacy siguiendo patrones de diseño limpios."
                    ]
                }
            ]
        }
    ]
};
