export interface Example {
    language: string;
    code: string;
}

export interface Resource {
    id: string;
    type: 'diagram' | 'image' | 'infographic';
    url: string;
    title: string;
    description?: string;
}

export interface Session {
    id: number;
    title: string;
    content: string;
    examples?: Example[];
    exercises?: string[];
    resources?: Resource[];
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
                    content: `### Arquitectura R/3 y ECC: El Modelo de 3 Capas
El entendimiento de la arquitectura de SAP es fundamental para cualquier consultor ABAP. El sistema se basa en un modelo de **Tres Capas** que garantiza escalabilidad y rendimiento:

#### 1. Capa de Presentación (Presentation Layer)
Es la interfaz con la que interactúa el usuario. Comúnmente es el **SAP GUI**, pero también puede ser un navegador web o una aplicación móvil. Su función es recibir las entradas del usuario y mostrar los resultados procesados.

#### 2. Capa de Aplicación (Application Layer)
Es el "cerebro" del sistema. Aquí reside el **Servidor de Aplicaciones ABAP (AS ABAP)**.
- **ABAP Dispatcher**: Recibe las solicitudes de la capa de presentación y las distribuye entre los procesos de trabajo disponibles.
- **Work Processes (Procesos de Trabajo)**:
    - **Dialog (DIA)**: Procesa las pantallas interactivas.
    - **Background (BTC)**: Ejecuta programas en segundo plano (Jobs).
    - **Update (UPD)**: Gestiona las actualizaciones críticas en la base de datos (LUW).
    - **Spool (SPO)**: Gestiona la salida de impresión y documentos.

#### 3. Capa de Base de Datos (Database Layer)
Donde se almacenan físicamente todos los datos del sistema (tablas maestro, transaccionales, configuración). Puede ser **SAP HANA**, Oracle, SQL Server, etc. ABAP se comunica con ella mediante **Open SQL**.

### Transacciones Clave para el Desarrollador
- **SE80 (Object Navigator)**: La herramienta central para organizar el código en paquetes.
- **SE11 (ABAP Dictionary)**: Para definir la estructura de datos.
- **SE38 (ABAP Editor)**: Para la codificación lógica tradicional.`,
                    resources: [
                        {
                            id: 'arch-1',
                            type: 'diagram',
                            url: '/assets/sap_r3_architecture.png',
                            title: 'Arquitectura SAP R/3 (3 Capas)',
                            description: 'Diagrama técnico que muestra la interacción entre las capas de Presentación, Aplicación y Base de Datos.'
                        }
                    ],
                    exercises: [
                        "Dibuja el flujo de una petición desde el SAP GUI hasta la Base de Datos, identificando el Dispatcher.",
                        "En la transacción SM50, identifica cuántos Work Processes de tipo DIA y BTC están activos en tu servidor.",
                        "Configura el 'Pretty Printer' en la SE80 siguiendo el estándar de indentación de la empresa.",
                        "Explica la diferencia técnica entre un servidor de aplicaciones central y uno de diálogo.",
                        "Identifica en qué capa de la arquitectura se ejecutan las BAPIs."
                    ]
                },
                {
                    id: 2,
                    title: "S2: DDIC I - Tablas, Dominios y Elementos",
                    content: `### Diccionario de Datos (SE11): El Corazón de SAP
El Diccionario de Datos (DDIC) permite crear y gestionar definiciones de datos de forma centralizada. En ABAP, no creamos tablas "al vuelo", sino que seguimos una jerarquía estricta que garantiza la integridad.

#### La Jerarquía de Definición
1.  **Dominio (Domain)**: Es el nivel más bajo. Define las características **técnicas** del campo: tipo de dato (CHAR, NUMC, QUAN, etc.), longitud, decimales y valores fijos permitidos.
2.  **Elemento de Datos (Data Element)**: Define el significado **semántico**. Aquí se asocia el Dominio a un "Rol" (ej: ID de Cliente, Fecha de Factura). Contiene las etiquetas (Labels) que verá el usuario en las pantallas.
3.  **Campo de Tabla (Table Field)**: Es la implementación física en una tabla transparente. Se le asigna un Elemento de Datos.

#### Características Técnicas de las Tablas
Al crear una tabla Z, es vital configurar:
- **Clase de Entrega**: Generalmente 'A' para datos maestros y transaccionales.
- **Categoría de Tamaño**: Determina cuánto espacio inicial reserva la base de datos.
- **Buffering**: Permite cachear la tabla en el servidor de aplicaciones para mejorar el rendimiento en tablas que se leen mucho pero cambian poco.`,
                    resources: [
                        {
                            id: 'ddic-1',
                            type: 'diagram',
                            url: '/assets/sap_ddic_hierarchy.png',
                            title: 'Jerarquía del Diccionario de Datos (DDIC)',
                            description: 'Relación técnica entre Dominios, Elementos de Datos y Campos de Tabla.'
                        }
                    ],
                    exercises: [
                        "Crea un Dominio ZESTADO con valores fijos: 'A' (Activo), 'I' (Inactivo), 'P' (Pendiente).",
                        "Crea un Elemento de Datos ZESTADO_E que use el dominio anterior y añade textos de cabecera en español.",
                        "Crea una tabla transparente ZUSUARIOS_SEI con campos: Mandante, ID_Usuario, Nombre, Estado y Fecha_Alta.",
                        "En la pestaña 'Technical Settings', activa el registro de logs de datos (Data Logging) y explica para qué sirve.",
                        "Inserta 3 registros manualmente desde la SE11 y visualízalos en la transacción SE16N."
                    ]
                },
                {
                    id: 3,
                    title: "S3: DDIC II - Vistas, Estructuras y Ayudas F4",
                    content: `### Objetos Complejos y Mejora de UX
Una vez dominadas las tablas, el siguiente paso es la abstracción y la ayuda al usuario.

#### 1. Vistas de Base de Datos (Database Views)
Permiten unir (Join) varias tablas en una sola entidad lógica. Son fundamentales para reportes complejos donde la información está distribuida (ej: Cabecera y Posiciones de pedido).

#### 2. Estructuras (Structures)
A diferencia de las tablas, las estructuras **no almacenan datos**. Se usan para definir tipos complejos en memoria, muy útiles para el paso de parámetros en funciones y clases.

#### 3. Ayudas de Búsqueda (Search Helps)
Son los famosos "Matches" o ayudas F4. 
- **Ayuda Elemental**: Basada en una sola tabla o vista.
- **Ayuda Colectiva**: Agrupa varias ayudas elementales, permitiendo al usuario buscar por diferentes criterios (ej: buscar cliente por nombre, por ciudad o por ID).`,
                    exercises: [
                        "Diseña una Database View que una la tabla MARA (Datos generales material) y MAKT (Textos de material).",
                        "Crea una Estructura ZS_DATOS_PERSONA que contenga nombre, apellido y DNI.",
                        "Crea una Ayuda de Búsqueda para tu tabla ZUSUARIOS_SEI que permita buscar por Nombre o por Estado.",
                        "Añade un campo de 'País' a tu tabla y asígnale la ayuda de búsqueda estándar H_T005.",
                        "Explica la diferencia entre un Include de Estructura y una Estructura Apéndice (Append Structure)."
                    ]
                },
                {
                    id: 4,
                    title: "S4: Lógica Procedural y Manejo de Tablas Internas",
                    content: `### El Motor de ABAP: Tablas Internas
Las tablas internas son la estructura de datos más importante en ABAP. Permiten almacenar y manipular grandes volúmenes de datos en la memoria del servidor de aplicaciones.

#### Tipos de Tablas Internas
1.  **Standard Tables**: Acceso secuencial. Ideales si vas a procesar todos los datos con un LOOP. Para búsquedas rápidas requieren un SORT y BINARY SEARCH.
2.  **Sorted Tables**: Siempre se mantienen ordenadas por su clave. Permiten búsquedas logarítmicas automáticas. Ideales para acceso frecuente por clave.
3.  **Hashed Tables**: Acceso mediante un algoritmo de Hash. El tiempo de respuesta es constante (O(1)) sin importar si la tabla tiene 10 o 1.000.000 de registros. Obligatorio definir una clave única.

#### Operaciones Clave
- **APPEND**: Añade al final (solo Standard).
- **INSERT**: Inserta respetando el índice o clave.
- **MODIFY**: Actualiza un registro existente.
- **READ TABLE**: Recupera un único registro.`,
                    resources: [
                        {
                            id: 'it-1',
                            type: 'diagram',
                            url: '/assets/sap_internal_tables.png',
                            title: 'Comparativa de Tablas Internas',
                            description: 'Gráfico comparativo de rendimiento y uso de tablas Standard, Sorted y Hashed.'
                        }
                    ],
                    exercises: [
                        "Declara una tabla interna de tipo Hashed para almacenar KPIs de ventas por cliente.",
                        "Demuestra con un ejemplo real la diferencia de tiempo entre un LOOP WHERE y un READ TABLE con clave.",
                        "Uso de FIELD-SYMBOLS para modificar el campo 'Fecha de Proceso' en una tabla de 10,000 registros.",
                        "Implementa una búsqueda binaria manual en una tabla Standard previamente ordenada.",
                        "Explica cuándo es preferible el uso de TABLES vs TYPES en la declaración de datos modernos."
                    ]
                },
                {
                    id: 5,
                    title: "S5: Modularización y Reutilización",
                    content: `### Clean Code ABAP: Modularización
La modularización consiste en dividir un programa complejo en unidades más pequeñas y manejables. Esto mejora la legibilidad, facilita la depuración y permite la reutilización de código.

#### Unidades de Modularización
- **Subrutinas (FORM/ENDFORM)**: Modularización local dentro del mismo programa. (Obsoletas en favor de clases locales).
- **Módulos de Función (Function Modules)**: Unidades globales que pueden ser llamadas desde cualquier lugar. Se gruman en **Grupos de Funciones**.
- **Métodos de Clase**: El estándar actual. Encapsulan lógica y datos.

#### Tipos de Parámetros
- **IMPORTING**: Datos que recibe el módulo.
- **EXPORTING**: Datos que devuelve.
- **CHANGING**: Datos que entran, se modifican y salen.
- **EXCEPTIONS**: Manejo de errores controlados.`,
                    resources: [
                        {
                            id: 'mod-1',
                            type: 'diagram',
                            url: '/assets/sap_modularization.png',
                            title: 'Conceptos de Modularización SAP',
                            description: 'Esquema de las diferentes unidades de modularización: Forms, Function Modules y Clases.'
                        }
                    ],
                    exercises: [
                        "Crea un Grupo de Funciones ZSEI_FINANCE y añade una función para el cálculo del IVA.",
                        "Implementa una excepción personalizada 'IVA_INVALIDO' y lánzala usando la sentencia RAISE.",
                        "Crea un subprograma FORM que reciba una tabla interna por REFERENCIA y otra por VALOR.",
                        "Migra una subrutina FORM a un método de una clase local dentro de un reporte.",
                        "Utiliza el Transactional RFC (tRFC) para enviar datos a otro sistema de forma asíncrona."
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
                    content: `### Consultas de Alto Rendimiento en HANA
Con la llegada de SAP HANA, la forma de escribir SQL en ABAP ha evolucionado. Ya no se trata solo de traer datos, sino de procesarlos en la base de datos (**Code-to-Data**).

#### Open SQL vs SQL Nativo
- **Open SQL**: Es el estándar de SAP. Garantiza que el código funcione en cualquier base de datos (DB Independency).
- **SQL Moderno (v7.40+)**: Permite el uso de comas entre campos, @ para variables host, y expresiones complejas dentro del SELECT.

#### Claves de Performance
1.  **FOR ALL ENTRIES**: La alternativa eficiente a los SELECT dentro de un LOOP. Evita el "chatty traffic" con la DB.
2.  **Agregaciones**: Usar SUM, MIN, MAX directamente en el SELECT para reducir el volumen de datos transferidos.
3.  **ST05**: El Performance Trace es la herramienta obligatoria para medir el tiempo de respuesta y los accesos a índice.`,
                    exercises: [
                        "Refactoriza un SELECT que use INNER JOIN con 4 tablas siguiendo los nuevos estándares de comas y @variables.",
                        "Uso de CASE... END dentro de un SELECT para categorizar clientes por volumen de ventas en tiempo real.",
                        "Demuestra el impacto en performance de un SELECT * vs SELECT con campos específicos en una tabla de 1 millón de registros.",
                        "Implementa una consulta que use COALESCE para manejar valores nulos en el resultado.",
                        "Realiza un análisis de traza en ST05 para identificar lecturas secuenciales costosas (Full Table Scan)."
                    ]
                },
                {
                    id: 7,
                    title: "S7: Programación Orientada a Objetos (OO ABAP) I",
                    content: `### OO ABAP: Clases e Instancias
ABAP Objects es la tecnología moderna para el desarrollo en SAP. Permite crear sistemas más mantenibles, flexibles y reusables mediante la encapsulación.

#### Componentes de una Clase
- **Atributos**: Los datos del objeto (pueden ser estáticos o de instancia).
- **Métodos**: El comportamiento del objeto.
- **Visibilidad**:
    - **PUBLIC**: Accesible desde cualquier lugar.
    - **PROTECTED**: Accesible solo por la clase y sus hijas.
    - **PRIVATE**: Solo accesible dentro de la propia clase.

#### Eventos
Permiten que un objeto notifique a otros ante cambios de estado, habilitando una arquitectura desacoplada.`,
                    resources: [
                        {
                            id: 'oo-1',
                            type: 'diagram',
                            url: '/assets/sap_oo_abap.png',
                            title: 'Conceptos de OO ABAP',
                            description: 'Principios fundamentales: Encapsulación, Herencia y Polimorfismo en ABAP.'
                        }
                    ],
                    exercises: [
                        "Diseña una clase ZCL_SEI_VEHICULO con atributos privados y métodos GET/SET públicos.",
                        "Implementa un Constructor que valide los datos de entrada al instanciar el objeto.",
                        "Uso de Atributos Estáticos para contar cuántas instancias de la clase se han creado en la sesión.",
                        "Crea una clase local dentro de un reporte que gestione la lógica de cálculo de impuestos.",
                        "Añade un nivel de visibilidad 'READ-ONLY' a un atributo público y explica su ventaja."
                    ]
                },
                {
                    id: 8,
                    title: "S8: OO ABAP II - Herencia, Interfaces y Polimorfismo",
                    content: `### Patrones de Diseño y Flexibilidad
La verdadera potencia de OO ABAP reside en la capacidad de extender comportamientos sin modificar el código original.

#### Herencia (Inheritance)
Permite crear jerarquías de clases. Una subclase hereda atributos y métodos de la superclase y puede **redefinir** (redefine) el comportamiento de los métodos.

#### Interfaces
Definen un "contrato". Cualquier clase que implemente una interfaz debe proveer la lógica para sus métodos. Esto permite que diferentes clases sean tratadas de forma uniforme (**Polimorfismo**).

#### Casting
- **Up-Casting**: Tratar a una subclase como si fuera su superclase (Siempre seguro).
- **Down-Casting**: Tratar a una superclase como una subclase específica (Requiere verificación con ?= o IS INSTANCE OF).`,
                    exercises: [
                        "Crea una Superclase ZCL_COMPROBANTE y subclases ZCL_FACTURA y ZCL_NOTA_CREDITO que redefinan el método 'CALCULAR_TOTAL'.",
                        "Define una Interface ZIF_PRINTABLE e impleméntala en clases de diferentes módulos (SD, MM).",
                        "Realiza un Up-Casting para procesar una tabla de superclases que contiene diferentes objetos hijos.",
                        "Implementa el patrón Singleton para asegurar que solo exista una instancia de un gestor de configuración.",
                        "Uso de la sentencia CAST para realizar un Down-Casting seguro capturando la excepción CX_SY_MOVE_CAST_ERROR."
                    ]
                },
                {
                    id: 9,
                    title: "S9: Reportes ALV con CL_SALV_TABLE",
                    content: `### Visualización Profesional: El Framework SALV
El ABAP List Viewer (ALV) es la forma estándar de presentar datos tabulares en SAP. El modelo de clases CL_SALV_TABLE es el preferido por su simplicidad y potencia.

#### Ventajas de SALV
- Orientado a Objetos 100%.
- Funcionalidades estándar (Filtros, Totalizaciones, Exportar a Excel) incluidas automáticamente.
- Menos código que el antiguo REUSE_ALV_GRID_DISPLAY.

#### Personalización
- **Functions**: Activar botones de sistema.
- **Columns**: Cambiar textos, colores y visibilidad de columnas.
- **Events**: Capturar clics en celdas (Hotspots) para navegación.`,
                    resources: [
                        {
                            id: 'ux-1',
                            type: 'diagram',
                            url: '/assets/sap_ux_comparison.png',
                            title: 'Comparativa de UX: ALV vs Dynpros',
                            description: 'Diferencias clave entre el reporte tabular (ALV) y la interfaz de usuario clásica (Dynpros).'
                        }
                    ],
                    exercises: [
                        "Desarrollar un reporte de stock que utilice SALV para mostrar datos de la tabla MARD.",
                        "Habilitar todas las funciones estándar del ALV (Sort, Filter, Export) con una sola línea de código.",
                        "Implementar un Hotspot en el campo 'Número de Documento' para que al hacer clic abra la transacción estándar (ej: VA03).",
                        "Cambiar el color de una celda dinámicamente si el valor supera un umbral de riesgo.",
                        "Configurar el ALV para que se muestre en modo cebra (Zebra pattern) y con títulos de columna personalizados."
                    ]
                },
                {
                    id: 10,
                    title: "S10: Dynpros e Interfaces Gráficas Tradicionales",
                    content: `### Programación de Diálogo (Screen Painter)
Aunque Fiori es el futuro, las Dynpros (Dynamic Programs) siguen siendo vitales para transacciones complejas en el backend.

#### El Ciclo de Vida de una Pantalla
1.  **PBO (Process Before Output)**: Lógica para preparar la pantalla antes de que el usuario la vea (ej: ocultar botones, llenar dropdowns).
2.  **PAI (Process After Input)**: Lógica que se dispara cuando el usuario interactúa (ej: clic en Guardar, presionar Enter).

#### Herramientas
- **Screen Painter (SE51)**: Para diseñar visualmente la disposición de campos.
- **Menu Painter (SE41)**: Para definir el Status GUI (Botones, Menús, Títulos).`,
                    exercises: [
                        "Crear una transacción Z con una pantalla que solicite datos de cabecera y tenga un botón de 'Validar'.",
                        "Implementar un campo de entrada con ayuda F4 personalizada integrada en la pantalla.",
                        "Uso de un TABLE CONTROL para permitir la entrada de múltiples líneas de datos.",
                        "Configurar un STATUS GUI con un botón de salida (F3) y uno de cancelación (F12).",
                        "Implementar el cambio dinámico de atributos de campo (SCREEN-ACTIVE, SCREEN-INPUT) en el evento PBO."
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
                    content: `### Automatización de Transacciones: Batch Input
El Batch Input es la técnica clásica para cargar datos masivos en SAP simulando la interacción de un usuario con las pantallas. Es ideal para transacciones que no disponen de BAPIs.

#### Flujo de SHDB
1.  **Grabación**: Se registran todas las pulsaciones de teclas, campos llenados y OK-Codes.
2.  **Generación de Programa**: SHDB genera un esqueleto de código que llena una tabla interna de tipo ` + "`" + `BDCDATA` + "`" + `.
3.  **Ejecución**: Se procesa mediante ` + "`" + `CALL TRANSACTION` + "`" + ` o creando una Carpeta de Batch Input (SM35).

#### Modos de Ejecución
- **'A' (All)**: Muestra todas las pantallas (Modo Debug).
- **'N' (No-Display)**: Ejecución rápida en fondo.
- **'E' (Error)**: Solo se detiene si ocurre un error dinámico.`,
                    exercises: [
                        "Realiza una grabación de la transacción MM01 (Creación de Material) y genera su tabla BDCDATA única.",
                        "Crea un reporte que lea un archivo Excel y cargue 50 materiales usando Call Transaction en modo 'N'.",
                        "Implementa el manejo de mensajes capturando la estructura BDCMSGCOLL para generar un log detallado.",
                        "Explica por qué no se debe usar Batch Input para transacciones que tienen controles ENJOY (ej: ME21N).",
                        "Desarrollar un programa que genere una sesión en la SM35 para su posterior procesamiento manual."
                    ]
                },
                {
                    id: 12,
                    title: "S12: BAPIs y Commits Transaccionales",
                    content: `### El Estándar Moderno de Integración
Las **BAPIs (Business Application Programming Interfaces)** son métodos de objetos de negocio de SAP (BOR) que permiten la comunicación estandarizada.

#### Características de una BAPI
- **Independencia**: Pueden ser llamadas externamente vía RFC.
- **Integración**: Realizan todas las validaciones de negocio del estándar.
- **Retorno**: Siempre devuelven una tabla de mensajes (RETURN).

#### El Ciclo Transaccional
En ABAP, las BAPIs no hacen un COMMIT automático. Es responsabilidad del programador llamar a BAPI_TRANSACTION_COMMIT si el resultado fue exitoso, o BAPI_TRANSACTION_ROLLBACK si hubo errores.`,
                    resources: [
                        {
                            id: 'int-1',
                            type: 'diagram',
                            url: '/assets/sap_integration.png',
                            title: 'Integración SAP: RFC y BAPI',
                            description: 'Flujo de comunicación entre sistemas externos y el core de SAP mediante RFCs y BAPIs.'
                        }
                    ],
                    exercises: [
                        "Crear un Pedido de Compras (PO) funcional usando la BAPI_PO_CREATE1.",
                        "Implementar un bloque de control que verifique si el tipo de mensaje de retorno es 'E' o 'A' antes de confirmar la transacción.",
                        "Utilizar la extensión BAPI (ExtensionIn) para llenar un campo 'Z' personalizado en una tabla estándar.",
                        "Diferenciar mediante un ejemplo práctico el uso de BAPI_MATERIAL_SAVEDATA vs Batch Input.",
                        "Realizar una lectura de datos maestros de cliente usando la BAPI_CUSTOMER_GETDETAIL2."
                    ]
                },
                {
                    id: 13,
                    title: "S13: User Exits y Customer Exits",
                    content: `### Modificando el Estándar (Modelo Legacy)
SAP permite inyectar código en procesos estándar. Los Exits son los puntos de entrada previstos para esto.

#### 1. User Exits (Ventas/SD)
Son subrutinas (Forms) vacías dentro de programas estándar (generalmente empiezan por ` + "`" + `MV45A...` + "`" + `). Requieren clave de acceso a objeto para su primera modificación.

#### 2. Customer Exits (SMOD/CMOD)
Son funciones predefinidas que SAP llama.
- **SMOD**: Donde SAP define el Exit.
- **CMOD**: Donde el cliente crea un "Proyecto de Ampliación" para activarlo.

#### Búsqueda de Exits
Se pueden localizar buscando la sentencia ` + "`" + `CALL CUSTOMER-FUNCTION` + "`" + ` en el código estándar.`,
                    exercises: [
                        "Localizar un User Exit que se dispare al guardar un pedido de ventas (VA01) en el programa MV45AFZZ.",
                        "Implementar una validación que impida guardar un pedido si el cliente no tiene dirección de correo.",
                        "Crear un Proyecto de Ampliación en CMOD para activar el Customer Exit de verificación de datos de cabecera de factura.",
                        "Demuestra cómo pasar datos desde un User Exit a una tabla Z para auditoría interna.",
                        "Explica la limitación de los Customer Exits frente al nuevo Enhancement Framework."
                    ]
                },
                {
                    id: 14,
                    title: "S14: Business Add-Ins (BAdIs)",
                    content: `### Extensibilidad Orientada a Objetos
Las BAdIs representan la evolución de los Exits hacia el mundo de los objetos. Una BAdI es esencialmente una Interfaz que SAP llama en puntos específicos.

#### Tipos de BAdIs
- **Classic BAdIs**: Se gestionan en la SE18/SE19. Basadas en clases de programas.
- **New BAdIs (Kernel)**: Integradas en el Kernel de SAP, mucho más rápidas y parte del Enhancement Framework.

#### Ventajas
- **Multi-implementación**: Varios consultores pueden implementar la misma BAdI sin pisarse.
- **Filtros**: Se puede activar la lógica solo para ciertos países o tipos de documento de forma nativa.`,
                    exercises: [
                        "Implementar la BAdI ME_PROCESS_PO_CUST para añadir una pestaña personalizada en la transacción de pedidos de compra.",
                        "Crear una implementación de BAdI con filtro para que solo se ejecute cuando el centro logístico sea '1000'.",
                        "Desarrollar una BAdI propia (Z) para permitir que otras aplicaciones modifiquen el cálculo de comisiones de SEI.",
                        "Comparar mediante código la llamada a una BAdI clásica vs una BAdI de Kernel (GET BADI).",
                        "Migrar una lógica de Customer Exit a una BAdI estándar si está disponible."
                    ]
                },
                {
                    id: 15,
                    title: "S15: Enhancement Framework (Implicit/Explicit)",
                    content: `### Modificación Total: Enhancement Framework
Es el sistema más potente de SAP para modificar código. Permite añadir lógica donde un Exit o BAdI no llega.

#### 1. Enhancement Implícito (Implicit)
Existen automáticamente al inicio y al final de cada Función, Método o Form. No requieren configuración previa de SAP.

#### 2. Enhancement Explícito (Explicit)
Puntos (Points) o Secciones (Sections) definidos explícitamente por los desarrolladores de SAP.
- **Enhancement Point**: Añade código.
- **Enhancement Section**: Permite REEMPLAZAR el código estándar por uno propio.

#### Enhancement de Clase
Permite añadir nuevos métodos o atributos a clases estándar de SAP sin modificarlas físicamente.`,
                    exercises: [
                        "Insertar un Enhancement Implícito al final del Function Module de validación de DNI.",
                        "Añadir un nuevo campo a una estructura estándar de SAP mediante un Append Structure y llenarlo vía Enhancement.",
                        "Uso de un Enhancement Section para anular el cálculo de costes estándar en un proceso específico de SEI.",
                        "Añadir un método público a la clase estándar de gestión de materiales mediante un Class Enhancement.",
                        "Organizar múltiples enhancements bajo un 'Composite Enhancement Implementation' para facilitar el transporte."
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
                    content: `### El Nuevo Paradigma: Code-to-Data
En la programación ABAP tradicional, traíamos grandes volúmenes de datos al Servidor de Aplicaciones para procesarlos. Con SAP HANA, la estrategia cambia: enviamos la lógica a la base de datos.

#### 1. Base de Datos In-Memory
HANA almacena los datos en memoria (RAM) y de forma columnar, permitiendo agregaciones y búsquedas miles de veces más rápidas que una base de datos de disco tradicional.

#### 2. Optimizaciones en ABAP
- **Agregaciones**: Usar SUM, AVG directamente en el SELECT.
- **AMDP**: Escribir procedimientos directamente en lenguaje SQL Script (nativo de HANA) desde el editor ABAP.
- **CDS**: El nuevo estándar de modelado de datos que traslada la complejidad a la base de datos.`,
                    resources: [
                        {
                            id: 'hana-1',
                            type: 'diagram',
                            url: '/assets/sap_hana_code_to_data.png',
                            title: 'Paradigma Code-to-Data',
                            description: 'Evolución del modelo tradicional hacia el procesamiento masivo en base de datos HANA.'
                        }
                    ],
                    exercises: [
                        "Diferenciar mediante un caso de uso real cuándo usar un SELECT tradicional vs una agregación en HANA.",
                        "Crear una AMDP (ABAP Managed Database Procedure) que realice un cálculo estadístico complejo sobre millones de registros.",
                        "Ejecutar el ABAP Test Cockpit (ATC) para identificar código que no es compatible con el upgrade a S/4HANA.",
                        "Implementar el uso de 'Fast Data Access' para transferencias masivas entre capas.",
                        "Optimizar una consulta lenta identificando campos que no aprovechan el almacenamiento columnar."
                    ]
                },
                {
                    id: 17,
                    title: "S17: Core Data Services (CDS Views)",
                    content: `### Modelado Semántico de Datos
Las **CDS Views** son mucho más que simples vistas de base de datos. Permiten definir modelos de datos ricos con lógica de negocio y metadatos.

#### Características Principales
- **DDL (Data Definition Language)**: Basado en SQL pero extendido con anotaciones (` + "`" + `@` + "`" + `).
- **Asociaciones**: Una evolución inteligente de los Joins que solo se ejecutan "on-demand" (Lazy Loading).
- **Anotaciones**: Permiten definir cómo se verá el dato en Fiori o qué capacidades de búsqueda tendrá sin escribir código UI.`,
                    exercises: [
                        "Crear una CDS View que exponga datos de facturación con anotaciones de moneda y unidad de medida.",
                        "Implementar una Asociación entre una tabla de cabecera y posiciones en una CDS.",
                        "Crear una Vía de Proyección de CDS para filtrar datos sensibles según el usuario conectado.",
                        "Utilizar Funciones de Agregación y Expresiones Case dentro de la definición de una CDS View.",
                        "Consumir una CDS View desde un ALV de SALV Table en un reporte tradicional."
                    ]
                },
                {
                    id: 18,
                    title: "S18: SAP Gateway y Servicios OData",
                    content: `### Abriendo SAP al Mundo
Para que una aplicación Fiori o externa consuma datos de SAP, usamos el protocolo **OData** (REST + JSON/XML).

#### Transaction SEGW (Gateway Service Builder)
1.  **Modelado**: Definir las Entidades (Estructuras de datos) y sus Relaciones.
2.  **Implementación**: Escribir la lógica ABAP en los métodos ` + "`" + `_GET_ENTITY` + "`" + ` (una sola) y ` + "`" + `_GET_ENTITYSET` + "`" + ` (lista de datos).
3.  **Registro**: Publicación del servicio para que sea accesible mediante una URL.`,
                    exercises: [
                        "Modelar una entidad 'Producto' en la SEGW con sus propiedades clave.",
                        "Implementar el método GET_ENTITYSET para devolver el stock de productos filtrado por centro logístico.",
                        "Implementar la operación CREATE para permitir la creación de incidencias desde una App externa.",
                        "Realizar pruebas de performance usando el Gateway Client y verificando el tiempo de respuesta del JSON.",
                        "Implementar el manejo de errores lanzando excepciones de negocio que lleguen como mensajes HTTP 400/500."
                    ]
                },
                {
                    id: 19,
                    title: "S19: ABAP RESTful Application Programming (RAP)",
                    content: `### El Futuro es RAP
El **ABAP RESTful Application Programming Model (RAP)** es el estándar actual para desarrollar aplicaciones en S/4HANA Cloud y On-Premise.

#### Capas de RAP
- **Data Modeling**: Uso intensivo de CDS Views.
- **Behavior Definition**: Definir qué puede hacer la app (Crear, Editar, Borrar).
- **Behavior Implementation**: Programar la lógica en clases ABAP.
- **Service Provisioning**: Exponer el servicio OData de forma automática.`,
                    resources: [
                        {
                            id: 'rap-1',
                            type: 'diagram',
                            url: '/assets/sap_rap_architecture.png',
                            title: 'Arquitectura SAP RAP',
                            description: 'Modelo de programación para aplicaciones Fiori optimizadas para HANA.'
                        }
                    ],
                    exercises: [
                        "Crear un objeto de negocio RAP completo (Managed o Unmanaged) para la gestión técnica de consultores SEI.",
                        "Definir Validaciones automáticas que se ejecuten antes de guardar datos en la base de datos.",
                        "Implementar una 'Action' personalizada para cambiar el estado de un registro (ej: Aprobar/Rechazar).",
                        "Configurar el 'Draft Handling' para permitir que el usuario guarde su trabajo parcialmente.",
                        "Crear una proyección de servicio para Fiori Elements y visualizarla en el Preview del ADT."
                    ]
                },
                {
                    id: 20,
                    title: "S20: Clean ABAP y Unit Testing",
                    content: `### Excelencia en el Desarrollo
Un consultor de Sistemas e Información S.A.S debe escribir código no solo funcional, sino **limpio y testeable**.

#### Clean ABAP
- Nombrado expresivo (No más ` + "`" + `LT_TAB` + "`" + `, mejor ` + "`" + `sales_orders` + "`" + `).
- Métodos pequeños y enfocados (Single Responsibility).
- Evitar variables globales y Side-Effects.

#### ABAP Unit
Permite automatizar las pruebas de tu lógica sin necesidad de interacción manual. Garantiza que futuros cambios no rompan lo que ya funciona (Regresión).`,
                    exercises: [
                        "Refactorizar un programa 'Espagueti' de 500 líneas dividiéndolo en una clase con métodos limpios.",
                        "Escribir una Clase de Test (AU) que valide el cálculo de impuestos con diferentes escenarios (Tax positive/negative).",
                        "Implementar el 'Mocking' de una base de datos para testear lógica sin depender de datos reales.",
                        "Configurar el ABAP Test Cockpit (ATC) para que bloquee transportes si el código no cumple con los estándares SEI.",
                        "Realizar una sesión de Code Review sobre un programa de un compañero enfocándose en la legibilidad."
                    ]
                }
            ]
        }
    ]
};
