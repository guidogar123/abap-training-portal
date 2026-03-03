import { useState } from 'react';
import CodeBlock from './CodeBlock';
import { Terminal, Book, Code, Image as ImageIcon, Maximize2, ExternalLink, X } from 'lucide-react';
import { Session, Resource } from '../data/courseContent';
import { motion, AnimatePresence } from 'framer-motion';

const ResourceGallery = ({ resources, onSelect }: { resources: Resource[], onSelect: (res: Resource) => void }) => {
    if (!resources || resources.length === 0) return null;

    return (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-12">
            {resources.map((res) => (
                <div key={res.id} className="group relative bg-slate-900/40 rounded-2xl border border-white/5 overflow-hidden hover:border-blue-500/30 transition-all duration-500 shadow-2xl">
                    <div className="aspect-video relative overflow-hidden bg-slate-800">
                        <img
                            src={res.url}
                            alt={res.title}
                            className="w-full h-full object-cover transition-transform duration-700 group-hover:scale-110"
                        />
                        <div className="absolute inset-0 bg-gradient-to-t from-slate-950 via-transparent to-transparent opacity-60" />

                        <div className="absolute top-4 right-4 flex gap-2 translate-y-2 opacity-0 group-hover:translate-y-0 group-hover:opacity-100 transition-all duration-300">
                            <button
                                onClick={() => onSelect(res)}
                                className="p-2 bg-black/50 backdrop-blur-md rounded-full text-white hover:bg-blue-500 transition-colors shadow-lg"
                                title="Expandir imagen"
                            >
                                <Maximize2 size={16} />
                            </button>
                        </div>

                        <div className="absolute bottom-4 left-4 right-4">
                            <span className="inline-block px-2 py-0.5 rounded-md bg-blue-500/80 text-[10px] text-white font-bold uppercase tracking-wider mb-2">
                                {res.type}
                            </span>
                            <h4 className="text-white font-bold text-lg leading-tight mb-1">{res.title}</h4>
                        </div>
                    </div>
                    {res.description && (
                        <div className="p-4 border-t border-white/5">
                            <p className="text-slate-400 text-sm leading-relaxed line-clamp-2">
                                {res.description}
                            </p>
                        </div>
                    )}
                </div>
            ))}
        </div>
    );
};

const MarkdownLite = ({ content }: { content: string }) => {
    const lines = content.split(/\r?\n/);

    return (
        <div className="markdown-content space-y-4 text-slate-300 leading-relaxed font-light">
            {lines.map((line, i) => {
                const tr = line.trim();
                if (!tr) return <div key={i} className="h-2"></div>;

                // Headers
                if (tr.startsWith('#### ')) {
                    return <h4 key={i} className="text-lg font-semibold text-purple-300 mt-6 mb-2 flex items-center gap-2">
                        <div className="w-1.5 h-1.5 rounded-full bg-purple-400"></div>
                        {tr.replace('#### ', '')}
                    </h4>;
                }
                if (tr.startsWith('### ')) {
                    return <h3 key={i} className="text-xl font-bold text-sky-400 mt-8 mb-4 pb-2 border-b border-slate-700/50 flex items-center gap-2">
                        {tr.replace('### ', '')}
                    </h3>;
                }

                // Lists
                if (/^\d+\.\s/.test(tr)) {
                    return (
                        <div key={i} className="ml-4 pl-4 border-l-2 border-indigo-500/30 py-1 my-2 text-slate-200">
                            {renderInlines(tr)}
                        </div>
                    );
                }
                if (tr.startsWith('- ') || tr.startsWith('* ')) {
                    return (
                        <div key={i} className="ml-6 flex items-start gap-3 my-2">
                            <span className="text-indigo-400 mt-1.5">•</span>
                            <span className="text-slate-300">{renderInlines(tr.replace(/^[-*]\s+/, ''))}</span>
                        </div>
                    );
                }

                return (
                    <p key={i} className="my-2">
                        {renderInlines(line)}
                    </p>
                );
            })}
        </div>
    );
};

const renderInlines = (text: string) => {
    const parts = text.split(/(\*\*.*?\*\*|`.*?`)/);
    return parts.map((part, idx) => {
        if (!part) return null;
        if (part.startsWith('**') && part.endsWith('**')) {
            return <strong key={idx} className="text-white font-bold">{part.slice(2, -2)}</strong>;
        }
        if (part.startsWith('`') && part.endsWith('`')) {
            return (
                <code key={idx} className="bg-slate-800/80 px-1.5 py-0.5 rounded text-amber-300 text-xs font-mono border border-white/10 mx-1">
                    {part.slice(1, -1)}
                </code>
            );
        }
        return part;
    });
};

interface LessonContentProps {
    session: Session;
    onTryInPlayground?: (code: string) => void;
}

const LessonContent = ({ session, onTryInPlayground }: LessonContentProps) => {
    const [selectedResource, setSelectedResource] = useState<Resource | null>(null);

    if (!session) return <div className="p-10 text-center text-slate-500">Selecciona una lección para comenzar</div>;

    return (
        <div className="max-w-4xl mx-auto pb-20 animate-in fade-in duration-700">
            <header className="mb-12">
                <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-blue-500/10 text-blue-400 text-xs font-medium mb-6 border border-blue-500/20">
                    <Book size={12} />
                    <span>Clase Magistral</span>
                </div>
                <h2 className="text-5xl font-extrabold text-white mb-6 tracking-tighter bg-clip-text text-transparent bg-gradient-to-r from-white to-slate-400">
                    {session.title}
                </h2>
                <div className="h-1 w-24 bg-gradient-to-r from-blue-500 to-purple-500 rounded-full" />
            </header>

            {/* Nueva Galería de Recursos Visuales */}
            {session.resources && session.resources.length > 0 && (
                <ResourceGallery
                    resources={session.resources}
                    onSelect={(res) => setSelectedResource(res)}
                />
            )}

            {/* Modal de Imagen / Lightbox */}
            <AnimatePresence>
                {selectedResource && (
                    <div className="fixed inset-0 z-[100] flex items-center justify-center p-4 md:p-12">
                        <motion.div
                            initial={{ opacity: 0 }}
                            animate={{ opacity: 1 }}
                            exit={{ opacity: 0 }}
                            onClick={() => setSelectedResource(null)}
                            className="absolute inset-0 bg-slate-900/90 backdrop-blur-xl"
                        />
                        <motion.div
                            initial={{ scale: 0.9, opacity: 0, y: 20 }}
                            animate={{ scale: 1, opacity: 1, y: 0 }}
                            exit={{ scale: 0.9, opacity: 0, y: 20 }}
                            className="relative max-w-6xl w-full max-h-full bg-slate-900 rounded-3xl overflow-hidden border border-white/10 shadow-3xl flex flex-col"
                        >
                            <div className="flex justify-between items-center p-6 border-b border-white/5 bg-slate-900/50 backdrop-blur">
                                <div>
                                    <span className="inline-block px-2 py-0.5 rounded-md bg-blue-500/20 text-blue-400 text-[10px] font-bold uppercase tracking-wider mb-1">
                                        {selectedResource.type}
                                    </span>
                                    <h3 className="text-xl font-bold text-white leading-tight">{selectedResource.title}</h3>
                                </div>
                                <button
                                    onClick={() => setSelectedResource(null)}
                                    className="p-2 hover:bg-white/5 rounded-full text-slate-400 hover:text-white transition-colors"
                                >
                                    <X size={24} />
                                </button>
                            </div>
                            <div className="flex-1 overflow-auto p-2 bg-black/40 flex items-center justify-center">
                                <img
                                    src={selectedResource.url}
                                    alt={selectedResource.title}
                                    className="max-w-full max-h-[70vh] object-contain shadow-2xl"
                                />
                            </div>
                            {selectedResource.description && (
                                <div className="p-8 bg-slate-900">
                                    <p className="text-slate-300 leading-relaxed max-w-3xl">
                                        {selectedResource.description}
                                    </p>
                                </div>
                            )}
                        </motion.div>
                    </div>
                )}
            </AnimatePresence>

            <div className="glass-panel p-10 mb-12 shadow-2xl relative overflow-hidden group">
                <div className="absolute top-0 right-0 p-4 opacity-10 group-hover:opacity-20 transition-opacity">
                    <ImageIcon size={64} />
                </div>
                <MarkdownLite content={session.content} />
            </div>

            {session.examples && session.examples.length > 0 && (
                <div className="space-y-8 mt-16">
                    <div className="flex items-center justify-between">
                        <h3 className="text-2xl font-bold text-white flex items-center gap-3">
                            <div className="p-2 bg-green-500/20 rounded-lg">
                                <Code className="text-green-400" />
                            </div>
                            Laboratorio de Código
                        </h3>
                    </div>
                    {session.examples.map((ex, idx) => (
                        <div key={idx} className="bg-[#0b0e14] rounded-2xl border border-slate-800 shadow-2xl overflow-hidden group">
                            <div className="bg-slate-800/50 px-5 py-3 border-b border-white/5 text-xs text-slate-400 flex justify-between items-center">
                                <div className="flex items-center gap-2">
                                    <div className="flex gap-1.5">
                                        <div className="w-2.5 h-2.5 rounded-full bg-red-500/50" />
                                        <div className="w-2.5 h-2.5 rounded-full bg-amber-500/50" />
                                        <div className="w-2.5 h-2.5 rounded-full bg-green-500/50" />
                                    </div>
                                    <span className="ml-4 font-mono font-medium">demo_script_{idx + 1}.abap</span>
                                </div>
                                <div className="flex items-center gap-4">
                                    <span className="flex items-center gap-1"><ExternalLink size={12} /> SAP GUI Ready</span>
                                </div>
                            </div>
                            <CodeBlock
                                code={ex.code}
                                language={ex.language}
                                onTryInPlayground={onTryInPlayground}
                            />
                        </div>
                    ))}
                </div>
            )}

            {session.exercises && session.exercises.length > 0 && (
                <div className="mt-16 p-8 rounded-2xl bg-gradient-to-br from-indigo-900/30 via-slate-900/20 to-blue-900/10 border border-white/5 relative overflow-hidden">
                    <div className="absolute top-0 right-0 -m-8 w-64 h-64 bg-indigo-500/5 rounded-full blur-3xl" />
                    <h3 className="text-2xl font-bold text-indigo-300 mb-6 flex items-center gap-3 relative z-10">
                        <div className="p-2 bg-indigo-500/20 rounded-lg">
                            <Terminal size={22} className="text-indigo-400" />
                        </div>
                        Desafíos de la Sesión
                    </h3>
                    <div className="grid grid-cols-1 gap-4 relative z-10">
                        {session.exercises.map((ex, idx) => (
                            <div key={idx} className="flex items-start gap-4 p-4 rounded-xl bg-white/5 border border-white/5 hover:bg-white/10 transition-colors">
                                <span className="flex-shrink-0 flex items-center justify-center w-8 h-8 rounded-lg bg-indigo-500 text-white text-sm font-bold shadow-lg shadow-indigo-500/20 italic">
                                    #{(idx + 1).toString().padStart(2, '0')}
                                </span>
                                <span className="text-slate-300 py-1 leading-relaxed">{ex}</span>
                            </div>
                        ))}
                    </div>
                </div>
            )}
        </div>
    );
};

export default LessonContent;
